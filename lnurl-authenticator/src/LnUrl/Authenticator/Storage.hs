{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module LnUrl.Authenticator.Storage (
    -- * File format
    LogEntry (..),
    AuthIdentity (..),
    addLogEntry,
    getLog,

    -- * Generic encrypted storage
    encryptJsonFile,
    decryptJsonFile,
) where

import Control.Monad ((<=<))
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (cbcDecrypt, cbcEncrypt, cipherInit, makeIV)
import Crypto.Data.Padding (Format (PKCS7), pad, unpad)
import Crypto.Error (throwCryptoErrorIO)
import Crypto.KDF.Scrypt (Parameters (Parameters), generate)
import Crypto.Random (getRandomBytes)
import Data.Aeson (
    FromJSON,
    ToJSON,
    decode,
    encode,
    object,
    parseJSON,
    toJSON,
    withObject,
    (.:),
    (.=),
 )
import Data.Aeson.Types (Parser)
import Data.ByteArray (ScrubbedBytes, convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Lazy (Map, findWithDefault)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word32)
import Haskoin (XPrvKey, btc, xPrvFromJSON, xPrvToJSON)
import LnUrl.Auth (Action (..), actionText)
import qualified LnUrl.Auth as Auth
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

{- | Each LNURL-auth interaction generates a log entry recording the action and
 the timestamp.
-}
data LogEntry = LogEntry
    { action :: Maybe Action
    , timestamp :: UTCTime
    }
    deriving (Eq, Show)

instance FromJSON LogEntry where
    parseJSON = withObject "LogEntry" $ \obj ->
        LogEntry
            <$> (obj .: "action" >>= parseAction)
            <*> obj .: "timestamp"
      where
        parseAction = maybe (pure Nothing) $ maybe noParse (pure . Just) . Auth.parseAction
        noParse = fail "Unknown action"

instance ToJSON LogEntry where
    toJSON logEntry =
        object
            [ "action" .= (fmap actionText . action) logEntry
            , "timestamp" .= timestamp logEntry
            ]

type Version = Word32

-- | An identity is a private key together with an activity log
data AuthIdentity = AuthIdentity
    { xprv :: XPrvKey
    , activityLog :: Map Text [LogEntry]
    , version :: Version
    }
    deriving (Eq, Show)

instance FromJSON AuthIdentity where
    parseJSON = withObject "AuthIdentity" $ \obj ->
        AuthIdentity
            <$> (obj .: "xprv" >>= xPrvFromJSON btc)
            <*> obj .: "log"
            <*> (obj .: "version" >>= versionGuard)
      where
        versionGuard :: Word32 -> Parser Word32
        versionGuard = \case
            1 -> pure 1
            _ -> fail "Unknown version"

instance ToJSON AuthIdentity where
    toJSON AuthIdentity{xprv, activityLog, version} =
        object
            [ "xprv" .= xPrvToJSON btc xprv
            , "log" .= activityLog
            , "version" .= version
            ]

-- | Add an entry to the activity log for a domain
addLogEntry :: String -> Maybe Action -> AuthIdentity -> IO AuthIdentity
addLogEntry domain possibleAction a@AuthIdentity{activityLog} = do
    now <- getCurrentTime
    let logEntry = LogEntry possibleAction now
    pure a{activityLog = Map.insertWith (<>) (Text.pack domain) [logEntry] activityLog}

-- | Retrieve the activity log for a particular domain
getLog :: String -> AuthIdentity -> [LogEntry]
getLog domain AuthIdentity{activityLog} =
    findWithDefault mempty (Text.pack domain) activityLog

{- | Encrypt a JSON blob to a file, formatted like: @<SALT><IV><CIPHERTEXT>@ where

 * @SALT@ is 32 bytes used for key derivation
 * @IV@ is the 16 byte AES-256 iv
 * @CIPHERTEXT@ is the encrypted plaintext, padded with 16-byte PKCS7
-}
encryptJsonFile ::
    ToJSON a =>
    -- | User key
    ScrubbedBytes ->
    -- | Path to ident file
    FilePath ->
    -- | Data to encrypt
    a ->
    IO ()
encryptJsonFile userKey path value = do
    createDirectoryIfMissing True (takeDirectory path)
    salt <- getRandomBytes 32
    let key = stretchKey salt userKey
    aes <- throwCryptoErrorIO $ cipherInit @AES256 key
    Just iv <- makeIV <$> getRandomBytes @_ @ByteString 16
    BS.writeFile path $
        salt
            <> convert iv
            <> ( cbcEncrypt aes iv . pad paddingScheme
                    . BSL.toStrict
                    . encode
               )
                value

-- | Extract a JSON payload from a file.  See 'encryptJsonFile' for the layout.
decryptJsonFile ::
    FromJSON a =>
    -- | User key
    ScrubbedBytes ->
    FilePath ->
    IO (Maybe a)
decryptJsonFile userKey path = do
    content <- BS.readFile path

    let salt = BS.take 32 content
        Just iv = (makeIV . BS.take 16 . BS.drop 32) content
        ciphertext = BS.drop 48 content
        key = stretchKey salt userKey

    if BS.length content < 48
        then pure Nothing
        else do
            aes <- throwCryptoErrorIO $ cipherInit @AES256 key
            pure . (decode . BSL.fromStrict <=< unpad paddingScheme) $
                cbcDecrypt aes iv ciphertext

{- | Stretch a key using scrypt
 <https://blog.filippo.io/the-scrypt-parameters/>
-}
stretchKey ::
    -- | Salt
    ByteString ->
    -- | Pasword
    ScrubbedBytes ->
    ByteString
stretchKey = generate params
  where
    params = Parameters (2 ^ (20 :: Int)) 8 1 32

paddingScheme :: Format
paddingScheme = PKCS7 16
