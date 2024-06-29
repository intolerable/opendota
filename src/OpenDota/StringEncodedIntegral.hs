module OpenDota.StringEncodedIntegral
  ( StringEncodedIntegral(..)
  ) where

import Data.Functor.Contravariant
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import Web.HttpApiData

newtype StringEncodedIntegral a =
  StringEncodedIntegral { fromStringEncodedIntegral :: a }

instance Integral a => FromHttpApiData (StringEncodedIntegral a) where
  parseUrlPiece t = parseUrlPiece t >>= fromText
  parseHeader t = parseHeader t >>= fromText
  parseQueryParam t = parseQueryParam t >>= fromText

instance Integral a => ToHttpApiData (StringEncodedIntegral a) where
  toUrlPiece = toUrlPiece . toText
  toEncodedUrlPiece = toEncodedUrlPiece . toText
  toHeader = toHeader . toText
  toQueryParam = toQueryParam . toText
  toEncodedQueryParam = toEncodedQueryParam . toText

instance Integral a => FromJSON (StringEncodedIntegral a) where
  parseJSON = withText "StringEncodedIntegral" \t ->
    either (fail . Text.unpack) pure (fromText t)

instance Integral a => ToJSON (StringEncodedIntegral a) where
  toJSON = toJSON . toText
  toEncoding = toEncoding . toText

instance Integral a => FromJSONKey (StringEncodedIntegral a) where
  fromJSONKey = FromJSONKeyTextParser $
    either (fail . Text.unpack) pure . fromText

instance Integral a => ToJSONKey (StringEncodedIntegral a) where
  toJSONKey = contramap toText toJSONKey

fromText :: Integral a => Text -> Either Text (StringEncodedIntegral a)
fromText t =
  case Data.Text.Read.signed Data.Text.Read.decimal t of
    Left err -> Left $ Text.pack err
    Right (res, "") -> Right $ StringEncodedIntegral res
    Right (_, _rest) -> Left "input remaining"

toText :: Integral a => StringEncodedIntegral a -> Text
toText (StringEncodedIntegral i) =
  Text.Lazy.toStrict $ Builder.toLazyText $ Builder.decimal i
