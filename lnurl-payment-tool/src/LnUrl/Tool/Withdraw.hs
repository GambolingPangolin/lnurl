{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LnUrl.Tool.Withdraw (withdraw) where

import qualified Codec.Binary.Bech32 as Bech32
import Control.Exception (Exception, throwIO)
import Control.Monad ((>=>))
import qualified Data.Aeson as Ae
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import LnUrl.Tool.Utils (decodeBech32URL, jsonRequest, toSats)
import LnUrl.Withdraw (AckResponse (..), Response (..), SuccessResponse)
import qualified LnUrl.Withdraw as W
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as Http
import Network.URI (URI)

withdraw ::
    Manager ->
    -- | LNURL
    Text ->
    IO ()
withdraw mgr =
    either throwWithdrawException pure . decodeBech32URL
        >=> firstRequest mgr
        >=> getWithdrawURI
        >=> secondRequest mgr
  where

firstRequest :: Manager -> Text -> IO SuccessResponse
firstRequest mgr url = do
    TIO.putStrLn $ "Trying " <> url
    Http.parseRequest (Text.unpack url)
        >>= jsonRequest mgr
        >>= either throwWithdrawException onResponse
  where
    onResponse = \case
        Success x -> pure x
        ErrorResponse msg -> throwWithdrawException msg

type PaymentRequest = Text

getWithdrawURI :: SuccessResponse -> IO URI
getWithdrawURI successResponse = do
    TIO.putStrLn $
        Text.unwords
            [ "You can withdraw between"
            , (Text.pack . show . toSats) (W.minWithdrawable successResponse)
            , "sats and"
            , (Text.pack . show . toSats) (W.maxWithdrawable successResponse)
            , "sats"
            ]
    TIO.putStrLn "Please enter the payment request."
    paymentRequest <- TIO.getLine
    pure $ W.getCallbackURL successResponse paymentRequest Nothing

secondRequest :: Manager -> URI -> IO ()
secondRequest mgr withdrawURI =
    Http.parseRequest (show withdrawURI)
        >>= jsonRequest mgr
        >>= either throwWithdrawException onResponse
  where
    onResponse = \case
        AckSuccess -> TIO.putStrLn "Success!"
        AckError msg -> throwWithdrawException msg

newtype WithdrawException = WithdrawException Text
    deriving (Eq, Show)

instance Exception WithdrawException

throwWithdrawException :: Text -> IO a
throwWithdrawException = throwIO . WithdrawException
