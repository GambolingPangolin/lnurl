{-# LANGUAGE OverloadedStrings #-}

{- |
Module: LnUrl.Withdraw

See <https://github.com/fiatjaf/lnurl-rfc/blob/master/lnurl-withdraw.md>.

== Workflow

1. @LN WALLET@ makes @GET@ request
2. @LN SERVICE@ responds with 'Response' 'SuccessResponse'.
3. @LN WALLET@ get withdrawal amount from user.
4. @LN WALLET@ prepare and make @GET@ request using 'getCallbackURL'.
5. @LN SERVICE@ responds with 'AckResponse'.
-}
module LnUrl.Withdraw (
    -- * Client
    getCallbackURL,

    -- * Types
    Response (..),
    SuccessResponse (..),
    AckResponse (..),
) where

import Data.Aeson (
    FromJSON,
    ToJSON,
    object,
    parseJSON,
    toJSON,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import LnUrl (AckResponse (..), Response (..))
import LnUrl.Utils (JsonURI (..), (.=?))
import Network.URI (URI)
import Network.URI.Utils (addQueryParams, param)

-- | The initial GET request responds with 'Response' 'SuccessResponse'
data SuccessResponse = SuccessResponse
    { callback :: URI
    , challenge :: Text
    , defaultDescription :: Text
    , minWithdrawable :: Word64
    -- ^ millisatoshis
    , maxWithdrawable :: Word64
    -- ^ millisatoshis
    , balanceCheck :: Maybe URI
    -- ^ URL to use to make a subsequent LNURL-withdraw request, response with 'SuccessResponse'
    }
    deriving (Eq, Show)

instance FromJSON SuccessResponse where
    parseJSON = withObject "SuccessResponse" $ \obj ->
        SuccessResponse
            <$> (getJsonURI <$> obj .: "callback")
            <*> obj .: "k1"
            <*> obj .: "defaultDescription"
            <*> obj .: "minWithdrawable"
            <*> obj .: "maxWithdrawable"
            <*> (fmap getJsonURI <$> obj .:? "balanceCheck")

instance ToJSON SuccessResponse where
    toJSON response =
        object $
            [ "callback" .= (JsonURI . callback) response
            , "k1" .= challenge response
            , "defaultDescription" .= defaultDescription response
            , "minWithdrawable" .= minWithdrawable response
            , "maxWithdrawable" .= maxWithdrawable response
            ]
                <> catMaybes ["balanceCheck" .=? (JsonURI <$> balanceCheck response)]

-- | Use the first response to build the callback url
getCallbackURL ::
    SuccessResponse ->
    -- | Payment request
    Text ->
    -- | URL where @LN SERVICE@ can POST 'SuccessResponse' values to notify the wallet
    Maybe URI ->
    URI
getCallbackURL response thePaymentRequest balanceNotifyURI =
    addQueryParams (callback response) $
        catMaybes
            [ Just $ param "k1" (encodeUtf8 . challenge) response
            , Just $ param "pr" encodeUtf8 thePaymentRequest
            , param "balanceNotify" (encodeUtf8 . Text.pack . show) <$> balanceNotifyURI
            ]
