{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LnUrl (
    Response (..),
    AckResponse (..),
    NodeId,
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
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text

data AckResponse
    = AckSuccess
    | AckError Text
    deriving (Eq, Show)

instance FromJSON AckResponse where
    parseJSON = withObject "AckResponse" $ \obj ->
        obj .: "status" >>= \case
            "OK" -> pure AckSuccess
            "ERROR" -> AckError <$> obj .: "reason"
            other -> fail $ "Unknown status: " <> other

instance ToJSON AckResponse where
    toJSON = \case
        AckSuccess ->
            object ["status" .= ("OK" :: Text)]
        AckError msg ->
            object
                [ "status" .= ("ERROR" :: Text)
                , "reason" .= msg
                ]

type NodeId = ByteString

data Response a = Success a | ErrorResponse Text
    deriving (Eq, Show)

instance FromJSON a => FromJSON (Response a) where
    parseJSON v = withObject "ResponseContainer" inspect v
      where
        inspect obj =
            obj .:? "status" >>= \case
                Just "ERROR" -> ErrorResponse <$> obj .: "reason"
                Just other -> fail $ "Unknown status: " <> Text.unpack other
                Nothing -> Success <$> parseJSON v

instance ToJSON a => ToJSON (Response a) where
    toJSON = \case
        Success x -> toJSON x
        ErrorResponse reason ->
            object
                [ "status" .= ("ERROR" :: Text)
                , "reason" .= reason
                ]
