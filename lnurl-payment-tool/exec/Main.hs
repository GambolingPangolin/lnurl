{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<**>))
import Data.Text (Text)
import qualified Data.Text as Text
import LnUrl.Tool.Pay (pay)
import LnUrl.Tool.Withdraw (withdraw)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Options.Applicative as Opt

data Command = Withdraw Text | Pay Text

getCommand :: IO Command
getCommand = Opt.execParser $ Opt.info (opts <**> Opt.helper) desc
  where
    opts =
        Opt.hsubparser $
            Opt.command "withdraw" cmdWithdraw
                <> Opt.command "pay" cmdPay
    cmdWithdraw = Opt.info (Withdraw <$> optLNURL) $ Opt.progDesc "Withdraw workflow"
    cmdPay = Opt.info (Pay <$> optLNURL) $ Opt.progDesc "Pay workflow"
    optLNURL = fmap (dropProto . Text.pack) . Opt.strArgument $ Opt.metavar "LNURL"
    dropProto lnurl
        | "lightning:" == Text.take 10 lnurl = Text.drop 10 lnurl
        | otherwise = lnurl

    desc = Opt.progDesc "LNURL payment tool"

main :: IO ()
main = do
    mgr <- newTlsManager
    getCommand >>= \case
        Withdraw lnurl -> withdraw mgr lnurl
        Pay lnurl -> pay mgr lnurl
