{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- |
Module: LnUrl.Pay

See <https://github.com/fiatjaf/lnurl-rfc/blob/master/lnurl-pay.md>.

== Workflow

1. @LN WALLET@ decodes URL using 'getPayURL' and makes a @GET@ request.
2. @LN SERVICE@ responds with 'Response' 'SuccessResponse'.
3. @LN WALLET@ get parameters from the user.
4. @LN WALLET@ prepare a callback URL using 'getCallbackUrl'.
5. @LN SERVICE@ responds with 'Response' 'CallbackSuccessResponse'.
6. @LN WALLET@ verifies: (a) @h@ tag in 'paymentRequest' is @SHA256(metadata)@,
   (b) the amount in 'paymentRequest' matches the requested amount, and (c)
   signatures on @ChannelUpdate@ messages.
7. @LN WALLET@ pays invoice.
8. @LN WALLET@ after paying the invoice, execute the 'successAction' if defined.
-}
module LnUrl.Pay (
    -- * Client
    getPayURL,
    getCallbackUrl,
    decrypt,

    -- * Server
    encrypt,

    -- * Types
    Response (..),
    SuccessResponse (..),
    Metadata (..),
    CallbackSuccessResponse (..),
    Hop (..),
    SuccessAction (..),
    UrlAction (..),
    AesAction (..),
    AesError (..),
) where

import Control.Exception (Exception)
import Control.Monad (unless)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (cbcDecrypt, cbcEncrypt, makeIV)
import qualified Crypto.Cipher.Types as Crypto
import Crypto.Data.Padding (Format (PKCS7), pad, unpad)
import Crypto.Error (eitherCryptoError)
import Crypto.Random (getRandomBytes)
import Data.Aeson (
    FromJSON,
    ToJSON,
    Value (String),
    object,
    parseJSON,
    toJSON,
    withArray,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16')
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Haskoin (PubKey, exportPubKey)
import LnUrl (NodeId, Response (..))
import LnUrl.Utils (Base64 (..), JsonURI (..), (.=?))
import Network.URI (URI (..), parseURI, uriRegName, uriUserInfo)
import Network.URI.Utils (addQueryParams, param)

-- | Apply the LNURL-pay uri transform logic.  @LN SERVICE@ should respond to the resulting URL with 'Response'.
getPayURL :: URI -> URI
getPayURL theURI
    | Just userInfo <- uriUserInfo <$> uriAuthority theURI =
        theURI
            { uriScheme = if isOnion then "http:" else "https:"
            , uriAuthority = stompUserInfo <$> uriAuthority theURI
            , uriPath = "/.well-known/lnurlp/" <> takeWhile (`notElem` specialChars) userInfo
            , uriFragment = mempty
            }
    | otherwise = theURI
  where
    isOnion = drop (length domain - 6) domain == ".onion"
    Just domain = uriRegName <$> uriAuthority theURI
    stompUserInfo uriAuth = uriAuth{uriUserInfo = mempty}
    specialChars = [':', '@']

instance FromJSON SuccessResponse where
    parseJSON = withObject "LNURL-pay Response" $ \obj -> do
        tag <- obj .: "tag"
        unless (tag == ("payRequest" :: Text)) $ fail "Must have payRequest tag"
        SuccessResponse
            <$> (getJsonURI <$> obj .: "callback")
            <*> obj .: "maxSendable"
            <*> obj .: "minSendable"
            <*> (obj .: "metadata")
            <*> obj .:? "commentAllowed"

instance ToJSON SuccessResponse where
    toJSON response =
        object $
            [ "callback" .= (JsonURI . callback) response
            , "maxSendable" .= maxSendable response
            , "minSendable" .= minSendable response
            , "metadata" .= metadata response
            ]
                <> catMaybes ["commentAllowed" .=? commentAllowed response]

data SuccessResponse = SuccessResponse
    { callback :: URI
    , maxSendable :: Word64
    -- ^ millisatoshi
    , minSendable :: Word64
    -- ^ millisatoshi
    , metadata :: Text
    , commentAllowed :: Maybe Int
    }
    deriving (Eq, Show)

{- | The metadata array

 * Must contain a 'PlainText' value
 * May contain at most one of 'ImagePNG' or 'ImageJPEG'
 * May contain at most one of 'Email' or 'Ident'
-}
data Metadata
    = PlainText Text
    | ImagePNG ByteString
    | ImageJPEG ByteString
    | Email Text
    | Ident Text
    deriving (Eq, Show)

instance FromJSON Metadata where
    parseJSON = withArray "Metadata" $ parseTuple . toList
      where
        parseTuple = \case
            [String mimeType, String val] -> case mimeType of
                "text/plain" -> pure $ PlainText val
                "text/email" -> pure $ Email val
                "text/identifier" -> pure $ Ident val
                "image/png;base64" ->
                    either (fail . Text.unpack) (pure . ImagePNG)
                        . decodeBase64
                        $ encodeUtf8 val
                "image/jpeg;base64" ->
                    either (fail . Text.unpack) (pure . ImageJPEG)
                        . decodeBase64
                        $ encodeUtf8 val
                other -> fail $ "Unknown mimetype: " <> Text.unpack other
            _ -> fail "Expected tuple"

instance ToJSON Metadata where
    toJSON =
        \case
            PlainText x -> tuple "text/plain" x
            ImagePNG x -> tuple "image/png;base64" (encodeBase64 x)
            ImageJPEG x -> tuple "image/jpeg;base64" (encodeBase64 x)
            Email x -> tuple "text/email" x
            Ident x -> tuple "text/identifier" x
      where
        tuple :: Text -> Text -> Value
        tuple = curry toJSON

-- | Prepare a callback url to use to retrieve the payment request
getCallbackUrl ::
    SuccessResponse ->
    -- | amount (millisatoshis)
    Word64 ->
    -- | cache prevention
    Maybe ByteString ->
    -- | starting points (node ids)
    [NodeId] ->
    -- | comment
    Maybe Text ->
    -- | proof of payer
    Maybe PubKey ->
    Maybe URI
getCallbackUrl response amount maybeNonce fromNodes maybeComment maybeProofOfPayer
    | commentLengthOk = Just $ addQueryParams (callback response) params
    | otherwise = Nothing
  where
    commentLengthOk = fromMaybe True $ checkComment <$> commentAllowed response <*> maybeComment
    checkComment commentSizeBound = (<= commentSizeBound) . Text.length

    params =
        catMaybes
            [ Just $ param "amount" (BS8.pack . show) amount
            , param "nonce" encodeBase16' <$> maybeNonce
            , if null fromNodes
                then mempty
                else Just $ param "fromnodes" toNodeList fromNodes
            , param "comment" encodeUtf8 <$> maybeComment
            , param "proofofpayer" (exportPubKey True) <$> maybeProofOfPayer
            ]
    toNodeList = BS.intercalate ","

data CallbackSuccessResponse = CallbackSuccessResponse
    { paymentRequest :: Text
    , successAction :: Maybe SuccessAction
    , disposable :: Bool
    , routes :: [[Hop]]
    }
    deriving (Eq, Show)

instance FromJSON CallbackSuccessResponse where
    parseJSON = withObject "CallbackSuccessResponse" $ \obj ->
        CallbackSuccessResponse
            <$> obj .: "pr"
            <*> obj .:? "successAction"
            <*> (fromMaybe True <$> obj .:? "disposable")
            <*> obj .: "routes"

instance ToJSON CallbackSuccessResponse where
    toJSON successResponse =
        object
            [ "pr" .= paymentRequest successResponse
            , "successAction" .= successAction successResponse
            , "disposable" .= disposable successResponse
            , "routes" .= routes successResponse
            ]

data SuccessAction
    = Url UrlAction
    | Message Text
    | Aes AesAction
    deriving (Eq, Show)

instance FromJSON SuccessAction where
    parseJSON = withObject "SuccessAction" $ \obj ->
        obj .: "tag" >>= \case
            "url" ->
                fmap Url $
                    UrlAction
                        <$> obj .: "description"
                        <*> (obj .: "url" >>= maybe badUrl pure . parseURI)
            "message" -> Message <$> obj .: "message"
            "aes" ->
                fmap Aes $
                    AesAction
                        <$> obj .: "description"
                        <*> (getBase64 <$> obj .: "ciphertext")
                        <*> (getBase64 <$> obj .: "iv")
            other -> fail $ "Unknown tag: " <> Text.unpack other
      where
        badUrl = fail "Unable to parse url"

instance ToJSON SuccessAction where
    toJSON = \case
        Url urlAction ->
            object
                [ "tag" .= ("url" :: Text)
                , "description" .= urlDescription urlAction
                , "url" .= show (url urlAction)
                ]
        Message msg ->
            object
                [ "tag" .= ("message" :: Text)
                , "message" .= msg
                ]
        Aes aes ->
            object
                [ "tag" .= ("aes" :: Text)
                , "description" .= aesDescription aes
                , "ciphertext" .= (Base64 . ciphertext) aes
                , "iv" .= (Base64 . iv) aes
                ]

data UrlAction = UrlAction
    { urlDescription :: Text
    , url :: URI
    }
    deriving (Eq, Show)

data AesAction = AesAction
    { aesDescription :: Text
    , ciphertext :: ByteString
    , iv :: ByteString
    }
    deriving (Eq, Show)

data Hop = Hop
    { nodeId :: Text
    , channelUpdate :: ByteString
    }
    deriving (Eq, Show)

instance FromJSON Hop where
    parseJSON = withObject "Hop" $ \obj ->
        Hop
            <$> obj .: "nodeId"
            <*> (getBase64 <$> obj .: "channelUpdate")

instance ToJSON Hop where
    toJSON theRoute =
        object
            [ "nodeId" .= nodeId theRoute
            , "channelUpdate" .= (Base64 . channelUpdate) theRoute
            ]

-- | Use the payment preimage to build an encrypted payload
encrypt ::
    -- | Payment preimage
    ByteString ->
    -- | Description
    Text ->
    -- | Message to encrypt
    ByteString ->
    IO (Either AesError AesAction)
encrypt key aesDescription plaintext = do
    iv <- getRandomBytes 16
    let Just cryptoniteIV = makeIV iv
    pure $
        cipherInit key <&> \cipher ->
            AesAction
                { aesDescription
                , iv
                , ciphertext = cbcEncrypt cipher cryptoniteIV (pad paddingConfig plaintext)
                }

-- | Use the payment preimage to get the encrypted payload
decrypt ::
    -- | Payment preimage
    ByteString ->
    AesAction ->
    Either AesError ByteString
decrypt key aes = do
    cipher <- cipherInit key
    theIV <- maybe (Left IvError) Right $ makeIV (iv aes)
    maybe (Left PaddingError) pure
        . unpad paddingConfig
        $ cbcDecrypt cipher theIV (ciphertext aes)

cipherInit :: ByteString -> Either AesError AES256
cipherInit = first (const KeyError) . eitherCryptoError . Crypto.cipherInit @AES256

paddingConfig :: Format
paddingConfig = PKCS7 16

data AesError = KeyError | IvError | PaddingError
    deriving (Eq, Show)

instance Exception AesError
