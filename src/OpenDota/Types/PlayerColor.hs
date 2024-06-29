module OpenDota.Types.PlayerColor
  ( PlayerColor(..)
  ) where

import Fmt
import Control.Monad
import Data.Aeson
import Data.Text
import Data.Void
import Data.Word (Word8)
import Text.Megaparsec
import qualified Data.Text.Read

data PlayerColor = PlayerColor
  { r :: Word8
  , g :: Word8
  , b :: Word8
  } deriving (Show, Read, Eq, Ord)

instance FromJSON PlayerColor where
  parseJSON = withText "PlayerColor" \t ->
    case runParser playerColorParser "PlayerColor" t of
      Left err -> fail $ errorBundlePretty err
      Right res -> pure res

instance ToJSON PlayerColor where
  toJSON = toJSON . toText

playerColorParser :: Parsec Void Text PlayerColor
playerColorParser = do
  void "#"
  redChannel <- hexWord8
  greenChannel <- hexWord8
  blueChannel <- hexWord8
  pure $ PlayerColor redChannel greenChannel blueChannel
    where
      hexWord8 = do
        t <- takeP (Just "hexWord8") 2
        case Data.Text.Read.hexadecimal t of
          Left err -> fail err
          Right (res, "") -> pure res
          Right (_, unconsumed) ->
            fail $ "Unconsumed input: " <> show unconsumed

toText :: PlayerColor -> Text
toText PlayerColor{..} = do
  "#" +| hex r <> hex g <> hex b
  where
    hex = padLeftF 2 '0' . hexF
