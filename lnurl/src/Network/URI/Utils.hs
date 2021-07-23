module Network.URI.Utils (
    addQueryParams,
    param,
) where

import Data.ByteString (ByteString)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types (Query, QueryItem, parseQuery, renderQuery)
import Network.URI (URI, uriQuery)

addQueryParams :: URI -> Query -> URI
addQueryParams uri extraQuery = uri{uriQuery = newQuery}
  where
    newQuery = Text.unpack . decodeUtf8 $ renderQuery True fullQuery
    fullQuery = extraQuery <> oldQuery
    oldQuery = (parseQuery . encodeUtf8 . Text.pack . uriQuery) uri

param :: ByteString -> (a -> ByteString) -> a -> QueryItem
param label f x = (label, Just (f x))
