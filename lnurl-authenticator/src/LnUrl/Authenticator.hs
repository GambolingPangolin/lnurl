{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module LnUrl.Authenticator (
    handleInit,
    handleAuth,
    handleLog,
    handleList,
    AuthenticatorError (..),
) where

import Codec.Binary.Bech32 (DecodingError)
import qualified Codec.Binary.Bech32 as Bech32
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as Ae
import Data.Bifunctor (bimap)
import Data.ByteArray (ScrubbedBytes, convert)
import Data.Char (toLower)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (defaultTimeLocale, formatTime)
import Haskoin (makeXPrvKey)
import LnUrl.Auth (
    AckResponse (..),
    Action (Register),
    AuthUrl,
    action,
    actionText,
    authDomain,
    getSignedCallback,
    parseAuthUrl,
 )
import LnUrl.Authenticator.Storage (
    AuthIdentity (..),
    LogEntry,
    addLogEntry,
    decryptJsonFile,
    encryptJsonFile,
    getLog,
 )
import qualified LnUrl.Authenticator.Storage as S
import Network.HTTP.Client (httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Console.Haskeline (
    InputT,
    defaultSettings,
    getInputChar,
    outputStrLn,
    runInputT,
 )
import qualified System.Console.Haskeline as HL

-- | Create a new LNURL-auth vault
handleInit :: FilePath -> IO ()
handleInit identFile = do
    putStrLn $ "Creating a new identity file: " <> identFile
    xprv <- makeXPrvKey <$> getRandomBytes 32
    password <- passwordInit
    encryptJsonFile password identFile $ AuthIdentity{xprv, activityLog = mempty, version = 1}
    putStrLn "Success!"

passwordInit :: IO ScrubbedBytes
passwordInit = runInputT defaultSettings $ do
    mpassword <- HL.getPassword Nothing "Encryption password: "
    mconfirmation <- HL.getPassword Nothing "Confirm encryption password: "
    case (mpassword, mconfirmation) of
        (Just password, Just confirmation)
            | password == confirmation -> (pure . convert . encodeUtf8 . Text.pack) password
        _ -> liftIO . throwIO $ PasswordEntryError

getPassword :: InputT IO ScrubbedBytes
getPassword =
    maybe
        mempty
        ( convert
            . encodeUtf8
            . Text.pack
        )
        <$> HL.getPassword Nothing "Encryption password: "

-- | Run LNURL-auth against a bech32-encoded URL
handleAuth :: FilePath -> String -> IO ()
handleAuth identFile bech32Url = do
    putStrLn "Starting LNURL-auth flow"
    either throwIO onUrl $
        decodeBech32 (Text.pack bech32Url)
            >>= \case
                ("lnurl", Just bytes) -> pure . Text.unpack $ decodeUtf8 bytes
                _ -> Left Bech32Error
  where
    decodeBech32 =
        bimap
            Bech32DecodingError
            (bimap Bech32.humanReadablePartToText Bech32.dataPartToBytes)
            . Bech32.decodeLenient

    onUrl theURL = runInputT defaultSettings $ do
        password <- getPassword
        authIdent <- liftIO $ getIdentity password identFile
        authUrl <- liftIO $ maybe noParse pure $ parseAuthUrl theURL

        let domain = authDomain authUrl
            domainLog = getLog domain authIdent
            possibleAction = action authUrl

        response <- promptUser domain domainLog possibleAction
        when response . liftIO $
            handleCallback authIdent authUrl
                >>= \case
                    AckSuccess -> do
                        addLogEntry domain possibleAction authIdent
                            >>= encryptJsonFile password identFile
                        putStrLn "Success!"
                    AckError reason -> throwIO $ ResponseError reason

    noParse = throwIO AuthUrlParseError

getIdentity :: ScrubbedBytes -> FilePath -> IO AuthIdentity
getIdentity password identFile =
    maybe noIdent pure =<< decryptJsonFile @AuthIdentity password identFile
  where
    noIdent = throwIO IdentityFileError

handleCallback ::
    AuthIdentity ->
    AuthUrl ->
    IO AckResponse
handleCallback authIdent authUrl = do
    mgr <- newTlsManager
    parseRequest theCallback
        >>= fmap (Ae.decode . responseBody) . flip httpLbs mgr
        >>= maybe onResponseDecodingError pure
  where
    onResponseDecodingError = throwIO ResponseDecodingError
    theCallback = show $ getSignedCallback (xprv authIdent) authUrl

promptUser :: String -> [LogEntry] -> Maybe Action -> InputT IO Bool
promptUser domain domainLog possibleAction = do
    outputStrLn $ maybe noAction onAction possibleAction
    when (alreadyRegistered possibleAction) $
        outputStrLn "This domain has already registered."
    userAccepts <$> getInputChar "Proceed (y/n)? "
  where
    noAction = domain <> " did not supply an action."
    onAction theAction = domain <> " requests " <> Text.unpack (actionText theAction)

    alreadyRegistered = \case
        Just Register -> Just Register `elem` (S.action <$> domainLog)
        _ -> False

    userAccepts = (== Just 'y') . fmap toLower

handleLog :: FilePath -> String -> IO ()
handleLog identFile domain = runInputT defaultSettings $ do
    password <- getPassword
    authIdent <- liftIO $ getIdentity password identFile
    mapM_ outputLog $ getLog domain authIdent
  where
    outputLog logEntry = outputStrLn $ "* " <> timeToString logEntry <> " - " <> activityString logEntry
    timeToString = formatTime defaultTimeLocale "%F %T" . S.timestamp
    activityString = maybe "No action" (Text.unpack . actionText) . S.action

handleList :: FilePath -> IO ()
handleList identFile = runInputT defaultSettings $ do
    password <- getPassword
    theLog <- liftIO $ activityLog <$> getIdentity password identFile
    when (null theLog) $ outputStrLn "No domain registrations"
    mapM_ (outputStrLn . Text.unpack) $ Map.keys theLog

data AuthenticatorError
    = NoUrl
    | PasswordEntryError
    | AuthUrlParseError
    | IdentityFileError
    | Bech32Error
    | Bech32DecodingError DecodingError
    | ResponseDecodingError
    | ResponseError Text
    deriving (Eq, Show)

instance Exception AuthenticatorError
