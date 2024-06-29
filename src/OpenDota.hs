module OpenDota where

import Data.Maybe (fromMaybe)
import Numeric.Natural
import Data.Map (Map)
import Servant.Client
import Servant.Client.Core
import Network.HTTP.Client.TLS
import Streaming (lift)
import Streaming.Prelude (Stream, Of)
import Streaming.Prelude qualified as S

import OpenDota.API
import OpenDota.API qualified as API
import OpenDota.Types

data ConstantData = ConstantData
  { heroData :: Map HeroID Hero
  , itemData :: Map ItemKey Item
  , regionData :: Map RegionID Region
  , gameModeData :: Map GameModeID GameMode
  , lobbyTypeData :: Map LobbyTypeID LobbyType
  , playerColorData :: Map PlayerSlot PlayerColor
  } deriving (Show, Eq, Ord)

getConstantData :: RunClient m => m ConstantData
getConstantData = do
  let c = openDotaClient // constants
  ConstantData <$> (c // heroes)
               <*> (c // items)
               <*> (c // regions)
               <*> (c // gameModes)
               <*> (c // lobbyTypes)
               <*> (c // playerColors)

main :: IO ()
main = do
  cenv <- mkClientEnv <$> newTlsManager <*> pure openDotaBaseUrl
  runClientM clientMain cenv >>= \case
    Left err -> error $ show err
    Right () -> pure ()

clientMain :: Monad m => m ()
clientMain = pure ()

playerMatches :: forall m . (Monad m, RunClient m)
              => SteamID32 -> Stream (Of PlayerMatchSummary) m ()
playerMatches steamID32 = go Nothing
  where
    matchesPerRequest :: Natural
    matchesPerRequest = 250
    go :: Maybe Natural -> Stream (Of PlayerMatchSummary) m ()
    go off = do
      let opts = defaultPlayerMatchOptions
            { limit = Just matchesPerRequest
            , offset = off
            , significant = Just 0
            }
      ms <- lift $ API.openDotaClient
        // API.players
        /: steamID32
        // API.playerMatches
        /: opts
      S.each ms
      if null ms
        then pure ()
        else go (pure $ fromMaybe 0 off + matchesPerRequest)
