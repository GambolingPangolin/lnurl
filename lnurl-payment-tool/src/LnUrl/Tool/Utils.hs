{-# LANGUAGE OverloadedStrings #-}

module LnUrl.Tool.Utils (
    decodeBech32URL,
    jsonRequest,
    toSats,
) where

import qualified Codec.Binary.Bech32 as Bech32
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Ae
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word64)
import Network.HTTP.Client (Manager, Request)
import qualified Network.HTTP.Client as Http

decodeBech32URL :: Text -> Either Text Text
decodeBech32URL = either onDecodeError onResult . Bech32.decodeLenient
  where
    onResult (hr, dataPart)
        | Bech32.humanReadablePartToText hr == "lnurl"
        , Just bytes <- Bech32.dataPartToBytes dataPart =
            pure $ decodeUtf8 bytes
        | otherwise = Left "Unable to interpret LNURL"
    onDecodeError _ = Left "Unable to decode Bech32"

jsonRequest :: FromJSON a => Manager -> Request -> IO (Either Text a)
jsonRequest mgr request =
    first Text.pack . Ae.eitherDecode . Http.responseBody <$> Http.httpLbs request mgr

-- | Convert millisatoshi to satoshi
toSats :: Word64 -> Double
toSats = (/ 1000) . fromIntegral
