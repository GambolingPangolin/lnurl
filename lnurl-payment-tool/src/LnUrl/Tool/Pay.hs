{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LnUrl.Tool.Pay (pay) where

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.Internal as Bech32
import Control.Exception (Exception, throwIO)
import Control.Monad ((>=>))
import qualified Crypto.Hash as C
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as Ae
import Data.Bifunctor (first)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Word (Word64)
import LnUrl.Pay (
    CallbackSuccessResponse,
    Metadata (..),
    Response (..),
    SuccessAction (..),
    SuccessResponse,
 )
import qualified LnUrl.Pay as P
import LnUrl.Tool.Utils (decodeBech32URL, jsonRequest, toSats)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as Http
import Network.URI (URI)
import Text.Read (readMaybe)

pay :: Manager -> Text -> IO ()
pay mgr =
    either throwPayException pure . decodeBech32URL
        >=> firstRequest mgr
        >=> getCallbackKit
        >=> callback mgr
        >=> mapM_ onSuccessAction

-- | Initiate the payment flow
firstRequest :: Manager -> Text -> IO SuccessResponse
firstRequest mgr =
    Http.parseRequest . Text.unpack
        >=> jsonRequest mgr
        >=> either throwPayException getResponse

-- | Decode the metadata field and get the unique text metadata entry
getTextMetadata :: SuccessResponse -> IO Text
getTextMetadata =
    ( either (throwPayException . Text.pack) pure
        . Ae.eitherDecodeStrict
        . encodeUtf8
        . P.metadata
    )
        >=> onTextCandidates . mapMaybe toTextMetadata
  where
    toTextMetadata = \case
        PlainText text -> Just text
        _ -> Nothing
    onTextCandidates = \case
        [textMetadata] -> pure textMetadata
        _ -> throwPayException "Malformed response: expected exactly one text metadata"

data CallbackKit = CallbackKit
    { callbackURL :: URI
    , amount :: Word64
    , metadataHash :: ByteString
    }

-- | Interact with the user to get an amount and generate a callback URL
getCallbackKit :: SuccessResponse -> IO CallbackKit
getCallbackKit successResponse = do
    textMetadata <- getTextMetadata successResponse
    TIO.putStrLn $ "Text metadata: " <> textMetadata
    TIO.putStrLn $
        Text.unwords
            [ "Pay between"
            , (Text.pack . show . toSats) (P.minSendable successResponse)
            , "sats and"
            , (Text.pack . show . toSats) (P.maxSendable successResponse)
            ]
    TIO.putStrLn "Payment amount:"
    amount <- getLine >>= maybe (badAmount) (pure . fromSats) . readMaybe
    cachePrevention <- getRandomBytes 32
    callbackURL <-
        maybe noURI pure $
            P.getCallbackUrl
                successResponse
                amount
                (Just cachePrevention)
                mempty -- No route hints
                Nothing -- No comment
                Nothing -- No proof of payer
    pure
        CallbackKit
            { callbackURL
            , amount
            , metadataHash =
                BA.convert
                    . C.hashWith C.SHA256
                    . encodeUtf8
                    $ P.metadata successResponse
            }
  where
    badAmount = throwPayException "Unable to parse amount"
    noURI = throwPayException "Unable to construct URI"
    fromSats = floor @Double . (* 1000)

{- | Perform the callback and verify that the response is well-formed by checking that:
 1. The requested amount matches
 2. The "purpose of payment" field matches the metadata hash
-}
callback :: Manager -> CallbackKit -> IO (Maybe SuccessAction)
callback mgr callbackKit =
    (Http.parseRequest . show . callbackURL) callbackKit
        >>= jsonRequest mgr
        >>= either throwPayException getResponse
        >>= (verifyResponse <$> amount <*> metadataHash) callbackKit

verifyResponse ::
    -- | Expected amount
    Word64 ->
    -- | Expected metadata hash
    ByteString ->
    CallbackSuccessResponse ->
    IO (Maybe SuccessAction)
verifyResponse expectedAmount expectedHash callbackResponse
    | Left err <- requestFields =
        throwPayException $ "Unable to decode payment request: " <> err
    | Right (requestAmount, _) <- requestFields
    , expectedAmount /= requestAmount =
        throwPayException "Invalid amount"
    | Right (_, requestHash) <- requestFields
    , expectedHash /= requestHash =
        throwPayException $
            "Invalid metadata hash: expected "
                <> B16.encodeBase16 expectedHash
                <> " but got "
                <> B16.encodeBase16 requestHash
    | otherwise = do
        TIO.putStrLn $ "Please pay: " <> P.paymentRequest callbackResponse
        pure $ P.successAction callbackResponse
  where
    requestFields = parsePaymentRequest $ P.paymentRequest callbackResponse

parsePaymentRequest :: Text -> Either Text (Word64, ByteString)
parsePaymentRequest =
    first (const "Bech32 decoding error") . Bech32.decodeLenient
        >=> onBech32Values
  where
    onBech32Values (hr, dataPart) =
        (,)
            <$> getAmount (Bech32.humanReadablePartToText hr)
            <*> onDataWords (Bech32.dataPartToWords dataPart)

    noDataBytes = Left "Unable to parse data part"

    -- Drop the 35 bit timestamp then look for the h field
    onDataWords = getHash . drop 7

    getAmount hr
        | "lnbc" == Text.take 4 hr =
            (first Text.pack . TR.decimal) (Text.drop 4 hr) >>= toAmount
        | otherwise = Left "Unable to get amount"

    -- Convert the amount to millisatoshis
    toAmount (value, multiplier) = case multiplier of
        -- millibitcoin
        "m" -> pure $ 10 ^ (8 - 3 + 3) * value
        -- microbitcoin
        "u" -> pure $ 10 ^ (8 - 6 + 3) * value
        -- nanobitcoin
        "n" -> pure $ 10 ^ (8 - 9 + 3) * value
        -- picobitcoin
        "p" -> pure $ value `quot` 10
        _ -> Left "Unknown multiplier"

    getHash = \case
        ws@(w0 : l1 : l0 : remaining)
            -- All that remains is the signature
            | length ws <= 520 `quot` 5 -> noH
            -- Look for the h tag
            | w0 == toEnum 23
            , Just bytes <- hashBytes remaining ->
                pure . BS.pack $ take 32 bytes
            | otherwise -> getHash $ drop (32 * fromEnum l1 + fromEnum l0) remaining
        _ -> noH

    noH = Left "Unable to find h field"

    hashBytes ws = Bech32.toBase256 $ take 52 ws <> (toEnum <$> [0x00, 0x00, 0x00, 0x00])

-- | Display the success action so that the user can take further steps
onSuccessAction :: SuccessAction -> IO ()
onSuccessAction = \case
    Url url ->
        TIO.putStrLn $
            "URL action (" <> P.urlDescription url <> "): " <> (Text.pack . show . P.url) url
    Message text ->
        TIO.putStrLn $ "Message: " <> text
    Aes aes -> do
        TIO.putStrLn $ "Encrypted payload."
        TIO.putStrLn $ "Enter payment preimage:"
        preimage <- getPreimage
        either (throwPayException . Text.pack . show) onPlaintext (P.decrypt preimage aes)
  where
    onPlaintext bytes = TIO.putStrLn $ "Plaintext: " <> B64.encodeBase64 bytes
    getPreimage = BS8.getLine >>= either throwPayException pure . B16.decodeBase16

getResponse :: Response a -> IO a
getResponse = \case
    Success x -> pure x
    ErrorResponse msg -> throwPayException msg

newtype PayException = PayException Text
    deriving (Eq, Show)

instance Exception PayException

throwPayException :: Text -> IO a
throwPayException = throwIO . PayException
