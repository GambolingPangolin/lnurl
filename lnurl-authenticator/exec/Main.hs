{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Applicative (optional, (<**>))
import Control.Exception (throwIO)
import LnUrl.Authenticator (
    AuthenticatorError (..),
    handleAuth,
    handleInit,
    handleList,
    handleLog,
 )
import qualified Options.Applicative as Opt
import System.Clipboard (getClipboardString)
import System.Environment (getEnv)

data CliOptions = CliOptions
    { identityFile :: Maybe FilePath
    , command :: Command
    }
    deriving (Eq, Show)

data Command
    = Init
    | Auth AuthOptions
    | Log LogOptions
    | List
    deriving (Eq, Show)

newtype AuthOptions = AuthOptions {authOptionsURL :: Maybe String}
    deriving (Eq, Show)

newtype LogOptions = LogOptions {logOptionsDomain :: String}
    deriving (Eq, Show)

getOptions :: IO CliOptions
getOptions = Opt.execParser $ Opt.info (opts <**> Opt.helper) desc
  where
    opts = CliOptions <$> optional optIdentFile <*> optCommand

    optIdentFile =
        Opt.strOption $
            Opt.long "file"
                <> Opt.short 'f'
                <> Opt.help
                    ( "File in which to store identity (default: ~"
                        <> defaultIdentFilePath
                        <> ")"
                    )

    optCommand =
        Opt.hsubparser $
            Opt.command "init" cmdInit
                <> Opt.command "auth" cmdRun
                <> Opt.command "log" cmdLog
                <> Opt.command "list" cmdList

    cmdInit = Opt.info (pure Init) descInit
    descInit = Opt.progDesc "Initialize a new identity"

    cmdRun = Opt.info optsRun descRun
    descRun = Opt.progDesc "Identify to a service using LNURL-auth"
    optsRun = fmap Auth $ AuthOptions <$> optional optURL

    optURL =
        Opt.strOption $
            Opt.long "url"
                <> Opt.help "Service URL (default is to take the URL from the clipboard)"

    cmdLog = Opt.info optsLog descLog
    descLog = Opt.progDesc "Print an activity log for a domain"
    optsLog = fmap Log $ LogOptions <$> optDomain

    optDomain = Opt.strArgument $ Opt.metavar "DOMAIN"

    cmdList = Opt.info (pure List) descList
    descList = Opt.progDesc "List all domains with which we have interacted"

    desc = Opt.progDesc "LNURL-auth helper"

defaultIdentFile :: IO FilePath
defaultIdentFile = identFile <$> getEnv "HOME"
  where
    identFile = (<> defaultIdentFilePath)

defaultIdentFilePath :: FilePath
defaultIdentFilePath = "/.lnurl-auth/default.auth"

getIdentFile :: CliOptions -> IO FilePath
getIdentFile = maybe defaultIdentFile pure . identityFile

main :: IO ()
main = do
    options <- getOptions
    identFile <- getIdentFile options
    case command options of
        Init -> handleInit identFile
        Auth theOpts ->
            maybe getUrlFromClipboard pure (authOptionsURL theOpts) >>= handleAuth identFile
        Log theOpts -> handleLog identFile $ logOptionsDomain theOpts
        List -> handleList identFile
  where
    getUrlFromClipboard = getClipboardString >>= maybe noURL pure
    noURL = throwIO NoUrl
