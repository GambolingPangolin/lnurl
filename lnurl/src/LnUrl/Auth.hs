{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- |
 Module: LnUrl.Auth

 See <https://github.com/fiatjaf/lnurl-rfc/blob/master/lnurl-auth.md>.

 == Workflow

 1. @LN SERVICE@ Use 'mkAuthUrl' to generate.
 2. @LN WALLET@ Decode @LNURL@ and parse with 'parseAuthUrl'.
 3. @LN WALLET@ Display 'AuthUrl' details and get consent from user to authorize.
 4. @LN WALLET@ Use 'getSignedCallback' to prepare follow-up @GET@ request
 5. @LN SERVICE@ Responds with 'AckResponse'.
-}
module LnUrl.Auth (
    -- * Server
    mkAuthUrl,

    -- * Client
    parseAuthUrl,
    authDomain,
    getSignedCallback,

    -- * Types
    AuthUrl (..),
    Action (..),
    actionText,
    parseAction,
    AckResponse (..),

    -- * Utilities
    deriveLinkingKey,
    deriveLinkingPubKey,
) where

import Control.Monad (join, replicateM, (>=>))
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decodeBase16, encodeBase16')
import Data.Either.Extra (eitherToMaybe)
import Data.Serialize (getWord32be, runGet)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Haskoin (
    DerivPath,
    DerivPathI (..),
    Msg,
    PubKey,
    SecKey,
    XPrvKey,
    derivePath,
    derivePubKey,
    exportPubKey,
    exportSig,
    getMsg,
    getSecKey,
    listToPath,
    msg,
    signMsg,
    xPrvKey,
    (++/),
 )
import LnUrl (AckResponse (..))
import Network.HTTP.Types (parseQuery)
import Network.URI (URI, parseURI, uriAuthority, uriQuery, uriRegName)
import Network.URI.Utils (addQueryParams)

-- | Add the challenge and action parameters to a URI
mkAuthUrl :: URI -> Msg -> Maybe Action -> URI
mkAuthUrl origURI challenge maybeAction =
    addQueryParams
        origURI
        $ ("k1", Just hexChallenge) : maybe mempty (pure . mkActionParam) maybeAction
  where
    hexChallenge = encodeBase16' $ getMsg challenge
    mkActionParam theAction =
        ( "action"
        , (Just . encodeUtf8 . actionText) theAction
        )

data Action
    = Register
    | Login
    | Link
    | Auth
    deriving (Eq, Show)

actionText :: Action -> Text
actionText = \case
    Register -> "register"
    Login -> "login"
    Link -> "link"
    Auth -> "auth"

data AuthUrl = AuthUrl
    { uri :: URI
    , k1 :: Msg
    , action :: Maybe Action
    }
    deriving (Eq, Show)

authDomain :: AuthUrl -> String
authDomain = maybe mempty uriRegName . uriAuthority . uri

-- | Attempt to interpret a URL as an LNURL-auth URL
parseAuthUrl :: String -> Maybe AuthUrl
parseAuthUrl = parseURI >=> onURI
  where
    onURI uri = do
        let q = parseQuery . encodeUtf8 . Text.pack $ uriQuery uri
        k1 <- (join . lookup "k1") q >>= eitherToMaybe . decodeBase16 >>= msg
        action <-
            maybe
                (pure Nothing)
                (fmap pure . parseAction . decodeUtf8)
                $ (join . lookup "action") q
        pure AuthUrl{uri, k1, action}

parseAction :: Text -> Maybe Action
parseAction = \case
    "register" -> pure Register
    "login" -> pure Login
    "link" -> pure Link
    "auth" -> pure Auth
    _ -> Nothing

-- LNURL-auth uses a fixed path for the hashing key
hashingPath :: DerivPath
hashingPath = Deriv :| 138 :/ 0

-- Key used to calculate the key path for a domain
hashingKey :: XPrvKey -> SecKey
hashingKey = xPrvKey . derivePath hashingPath

-- | Derive the linking key from the domain
deriveLinkingKey ::
    XPrvKey ->
    -- | domain
    ByteString ->
    SecKey
deriveLinkingKey prv domain = xPrvKey $ derivePath linkingPath prv
  where
    linkingPath, linkingPathPrefix :: DerivPath
    linkingPathPrefix = Deriv :| 138
    Right linkingPath =
        fmap ((linkingPathPrefix ++/) . listToPath . take 4)
            . runGet (replicateM 4 getWord32be)
            . BS.take 16
            . convert @(HMAC SHA256)
            $ (hmac . getSecKey) (hashingKey prv) domain

-- | Derive a public signing key
deriveLinkingPubKey ::
    XPrvKey ->
    -- | domain
    ByteString ->
    PubKey
deriveLinkingPubKey prv = derivePubKey . deriveLinkingKey prv

-- | Use the linking key to sign a challenge
getSignedCallback ::
    -- | Root key
    XPrvKey ->
    AuthUrl ->
    -- | Callback url with client-provided LNURL-auth paramaters
    URI
getSignedCallback prv authUrl =
    addQueryParams
        (uri authUrl)
        [ ("sig", Just (encodeBase16' sig))
        , ("key", Just (encodeBase16' linkingKey))
        ]
  where
    Just domain = getDomain authUrl
    linkingPrvKey = deriveLinkingKey prv domain
    linkingKey = (exportPubKey True . derivePubKey) linkingPrvKey
    sig = (exportSig . signMsg linkingPrvKey . k1) authUrl

getDomain :: AuthUrl -> Maybe ByteString
getDomain = fmap (encodeUtf8 . Text.pack . uriRegName) . uriAuthority . uri
