module OpenDota.API
  ( OpenDotaAPI
  , openDotaAPI
  , OpenDotaEndpoints(..)
  , PlayersEndpoints(..)
  , PlayerMatchOptions(..)
  , defaultPlayerMatchOptions
  , ConstantsEndpoints(..)
  , openDotaBaseUrl
  , openDotaClient
  , (//)
  , (/:)
  ) where

import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Proxy
import GHC.Generics
import Servant.API
import Servant.Client
import Servant.Client.Core
import Numeric.Natural

import OpenDota.Types

type OpenDotaAPI = NamedRoutes OpenDotaEndpoints

openDotaAPI :: Proxy OpenDotaAPI
openDotaAPI = Proxy

data OpenDotaEndpoints mode = OpenDotaEndpoints
  { matches :: mode :-
      "matches" :> Capture "match_id" MatchID :> Get '[JSON] Match
  , players :: mode :-
      "players" :> Capture "account_id" SteamID32 :> NamedRoutes PlayersEndpoints
  , publicMatches :: mode :-
      "publicMatches" :>
        GetPublicMatches
  , request :: mode :-
      "request" :> NamedRoutes RequestEndpoints
  , constants :: mode :-
      "constants" :> NamedRoutes ConstantsEndpoints
  } deriving (Generic)

type GetPublicMatches =
  QueryParam' [Strict, Optional] "less_than_match_id" MatchID :>
  Get '[JSON] [MatchSummary]

data PlayersEndpoints mode = PlayersEndpoints
  { profile :: mode :-
      Get '[JSON] Player
  , wl :: mode :-
      "wl" :> Get '[JSON] WinLoseRecord
  , recentMatches :: mode :-
      "recentMatches" :> Get '[JSON] [PlayerMatchSummary]
  , playerMatches :: mode :-
      "matches" :> PlayerMatchOptions :> Get '[JSON] [PlayerMatchSummary]
  } deriving (Generic)

data PlayerMatchOptions = PlayerMatchOptions
  { limit :: Maybe Natural
  , offset :: Maybe Natural
  , heroID :: Maybe HeroID
  , gameModeID :: Maybe GameModeID
  , lobbyTypeID :: Maybe LobbyTypeID
  , significant :: Maybe Natural
  } deriving (Show, Eq, Ord)

defaultPlayerMatchOptions :: PlayerMatchOptions
defaultPlayerMatchOptions = PlayerMatchOptions
  { limit = Nothing
  , offset = Nothing
  , heroID = Nothing
  , gameModeID = Nothing
  , lobbyTypeID = Nothing
  , significant = Nothing
  }

type PlayerMatchOptions' api =
  QueryParam' [Strict, Optional] "limit" Natural :>
  QueryParam' [Strict, Optional] "offset" Natural :>
  QueryParam' [Strict, Optional] "hero_id" HeroID :>
  QueryParam' [Strict, Optional] "game_mode" GameModeID :>
  QueryParam' [Strict, Optional] "lobby_type" LobbyTypeID :>
  QueryParam' [Strict, Optional] "significant" Natural :>
  api

instance (RunClient m, HasClient m api) => HasClient m (PlayerMatchOptions :> api) where
  type Client m (PlayerMatchOptions :> api) = PlayerMatchOptions -> Client m api

  clientWithRoute (Proxy :: Proxy m) (Proxy :: Proxy (PlayerMatchOptions :> api)) req PlayerMatchOptions{..} =
    clientWithRoute (Proxy :: Proxy m) (Proxy :: Proxy (PlayerMatchOptions' api)) req limit offset heroID gameModeID lobbyTypeID significant

  hoistClientMonad (Proxy :: Proxy m) (Proxy :: Proxy (PlayerMatchOptions :> api)) nat c =
    hoistClientMonad (Proxy :: Proxy m) (Proxy :: Proxy api) nat . c

data RequestEndpoints mode = RequestEndpoints
  { byMatchID :: mode :-
      Capture "match_id" MatchID :> Post '[JSON] ()
  } deriving (Generic)

data ConstantsEndpoints mode = ConstantsEndpoints
  { heroes :: mode :-
      "heroes" :> Get '[JSON] (Map HeroID Hero)
  , items :: mode :-
      "items" :> Get '[JSON] (Map ItemKey Item)
  , regions :: mode :-
      "region" :> Get '[JSON] (Map RegionID Region)
  , gameModes :: mode :-
      "game_mode" :> Get '[JSON] (Map GameModeID GameMode)
  , lobbyTypes :: mode :-
      "lobby_type" :> Get '[JSON] (Map LobbyTypeID LobbyType)
  , playerColors :: mode :-
      "player_colors" :> Get '[JSON] (Map PlayerSlot PlayerColor)
  } deriving (Generic)

openDotaBaseUrl :: BaseUrl
openDotaBaseUrl =
  BaseUrl { baseUrlScheme = Https
          , baseUrlHost = "api.opendota.com"
          , baseUrlPort = 443
          , baseUrlPath = "api"
          }

openDotaClient :: RunClient m => Client m OpenDotaAPI
openDotaClient = clientIn openDotaAPI (Proxy :: Proxy m)

newtype OpenDotaClientM a = OpenDotaClientM (ClientM a)
  deriving ( Functor, Applicative, Monad
           , MonadIO
           )
