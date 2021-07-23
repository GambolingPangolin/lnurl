{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
 Module: LnUrl.Channel

 See <https://github.com/fiatjaf/lnurl-rfc/blob/master/lnurl-channel.md>.

 == Workflow

 1. @LN SERVICE@ provides a URL for @LNURL-channel@.
 2. @LN WALLET@ makes a @GET@ request to this URL.
 3. @LN SERVICE@ responds with 'Response' 'SuccessResponse'.
 4. @LN WALLET@ connects to the node at 'remoteNode'.
 5. @LN WALLET@ prepare and make a @GET@ request using 'proceed' (followed
    possibly by 'cancel').
 6. @LN SERVICE@ responds with 'AckResponse'.
 7. @LN WALLET@ awaits an @OpenChannel@ message.
-}
module LnUrl.Channel (
    -- * Client
    proceed,
    cancel,

    -- * Types
    NodeId,
    SuccessResponse (..),
    Response (..),
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
    (.=),
 )
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import LnUrl (AckResponse (..), NodeId, Response (..))
import LnUrl.Utils (Base16 (..), JsonURI (..), getBase16)
import Network.URI (URI)
import Network.URI.Utils (addQueryParams)

-- | @LN SERVICE@ responds with 'Response' 'SuccessResponse'
data SuccessResponse = SuccessResponse
    { -- | Remote node URI
      remoteNode :: ByteString
    , -- | Service URL
      callback :: URI
    , -- | Wallet identifier
      k1 :: ByteString
    }
    deriving (Eq, Show)

instance FromJSON SuccessResponse where
    parseJSON = withObject "SuccessResponse" $ \obj ->
        obj .: "tag" >>= \case
            "channelRequest" ->
                SuccessResponse
                    <$> (encodeUtf8 <$> obj .: "uri")
                    <*> (getJsonURI <$> obj .: "callback")
                    <*> (getBase16 <$> obj .: "k1")
            tag -> fail $ "Unknown tag: " <> tag

instance ToJSON SuccessResponse where
    toJSON success =
        object
            [ "uri" .= (decodeUtf8 . remoteNode) success
            , "callback" .= (show . callback) success
            , "k1" .= Base16 (k1 success)
            ]

{- | Create the URL for the follow up LNURL-channel request. @LN SERVICE@
 responds with 'AckResponse'.
-}
proceed :: SuccessResponse -> NodeId -> Bool -> URI
proceed payload theRemoteNode isPrivate =
    addQueryParams
        (callback payload)
        [ ("k1", Just $ k1 payload)
        , ("remoteid", Just theRemoteNode)
        , ("private", Just $ bool "0" "1" isPrivate)
        ]

{- | Create the URL to cancel a LNURL-channel request. @LN SERVICE@ responds
 with 'AckResponse'.
-}
cancel :: SuccessResponse -> NodeId -> URI
cancel payload theRemoteNode =
    addQueryParams
        (callback payload)
        [ ("k1", Just $ k1 payload)
        , ("remoteid", Just theRemoteNode)
        , ("cancel", Just "1")
        ]
