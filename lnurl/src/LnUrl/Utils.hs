{-# LANGUAGE OverloadedStrings #-}

module LnUrl.Utils (
    Base16 (..),
    Base64 (..),
    JsonURI (..),
    (.=?),
) where

import Data.Aeson (
    FromJSON,
    Key,
    ToJSON,
    Value,
    parseJSON,
    toJSON,
    withText,
    (.=),
 )
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Network.URI (URI, parseURI)

newtype Base16 = Base16 {getBase16 :: ByteString}
    deriving (Eq, Show)

instance FromJSON Base16 where
    parseJSON =
        withText "Base16" $
            either (fail . Text.unpack) (pure . Base16)
                . decodeBase16
                . encodeUtf8

instance ToJSON Base16 where
    toJSON = toJSON . encodeBase16 . getBase16

newtype Base64 = Base64 {getBase64 :: ByteString}
    deriving (Eq, Show)

instance FromJSON Base64 where
    parseJSON =
        withText "Base64" $
            either (fail . Text.unpack) (pure . Base64)
                . decodeBase64
                . encodeUtf8

instance ToJSON Base64 where
    toJSON = toJSON . encodeBase64 . getBase64

newtype JsonURI = JsonURI {getJsonURI :: URI}
    deriving (Eq, Show)

instance FromJSON JsonURI where
    parseJSON = withText "JsonURI" $ maybe noParse (pure . JsonURI) . parseURI . Text.unpack
      where
        noParse = fail "Unable to parse URI"

instance ToJSON JsonURI where
    toJSON = toJSON . show . getJsonURI

(.=?) :: ToJSON a => Key -> Maybe a -> Maybe (Key, Value)
k .=? mv = (k .=) <$> mv
