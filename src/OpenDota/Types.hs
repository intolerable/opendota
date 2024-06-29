module OpenDota.Types
  ( MatchID(..)
  , Match(..)
  , MatchPlayer(..)

  , SteamID32(..)
  , SteamID64(..)

  , MatchSummary(..)

  , Player(..)
  , Profile(..)
  , WinLoseRecord(..)

  , PlayerMatchSummary(..)
  , Faction(..)
  , PlayerSlot(..)

  , HeroID(..)
  , Hero(..)

  , ItemID(..)
  , ItemKey(..)
  , Item(..)

  , RegionID(..)
  , Region(..)

  , GameModeID(..)
  , GameMode(..)

  , LobbyTypeID(..)
  , LobbyType(..)

  , PlayerColor(..)
  ) where

import Data.Aeson
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Generics
import Numeric.Natural
import Web.HttpApiData

import OpenDota.StringEncodedIntegral
import OpenDota.Types.PlayerColor
import OpenDota.Types.PlayerSlot

newtype MatchID = MatchID Int64
  deriving ( Show, Read, Eq, Ord
           , ToHttpApiData, FromHttpApiData
           , ToJSON, FromJSON
           )

data Match = Match
  { matchID :: MatchID
  , players :: [MatchPlayer]
  , radiantWin :: Bool
  , duration :: NominalDiffTime
  , startTime :: UTCTime
  } deriving (Show, Eq, Ord)

instance FromJSON Match where
  parseJSON = withObject "Match" $ \o -> do
    Match <$> o .: "match_id"
          <*> o .: "players"
          <*> o .: "radiant_win"
          <*> o .: "duration"
          <*> (posixSecondsToUTCTime <$> o .: "start_time")

instance ToJSON Match where
  toJSON Match{..} = object
    [ "match_id" .= matchID
    , "players" .= players
    , "radiant_win" .= radiantWin
    , "duration" .= duration
    , "start_time" .= utcTimeToPOSIXSeconds startTime
    ]

data MatchPlayer = MatchPlayer
  { accountID :: Maybe SteamID32
  , playerSlot :: PlayerSlot
  , heroID :: HeroID
  , kills :: Natural
  , deaths :: Natural
  , assists :: Natural
  , goldPerMinute :: Natural
  , xpPerMinute :: Natural
  , xpT :: Maybe [Natural]
  , gT :: Maybe [Natural]
  } deriving (Show, Eq, Ord)

instance FromJSON MatchPlayer where
  parseJSON = withObject "MatchPlayer" \o ->
    MatchPlayer <$> o .:? "account_id"
                <*> o .: "player_slot"
                <*> o .: "hero_id"
                <*> o .: "kills"
                <*> o .: "deaths"
                <*> o .: "assists"
                <*> o .: "gold_per_min"
                <*> o .: "xp_per_min"
                <*> o .:? "xp_t"
                <*> o .:? "gold_t"

instance ToJSON MatchPlayer where
  toJSON MatchPlayer{..} = object
    [ "account_id" .= accountID
    , "player_slot" .= playerSlot
    , "hero_id" .= heroID
    , "kills" .= kills
    , "deaths" .= deaths
    , "assists" .= assists
    , "gold_per_min" .= goldPerMinute
    , "xp_per_min" .= xpPerMinute
    , "xp_t" .= xpT
    , "gold_t" .= gT
    ]

newtype SteamID32 = SteamID32 Int32
  deriving ( Show, Read, Eq, Ord
           , ToHttpApiData, FromHttpApiData
           , FromJSON, ToJSON
           )

newtype SteamID64 = SteamID64 Int64
  deriving ( Show, Read, Eq, Ord
           , ToHttpApiData, FromHttpApiData
           )
  deriving (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
    via (StringEncodedIntegral Int64)

data MatchSummary = MatchSummary
  { matchID :: MatchID
  , radiantWin :: Bool
  , startTime :: UTCTime
  , duration :: NominalDiffTime
  , lobbyType :: LobbyTypeID
  , gameMode :: GameModeID
  , radiantTeam :: [HeroID] -- maybe?
  , direTeam :: [HeroID] -- maybe?
  } deriving (Show, Eq, Ord)

instance FromJSON MatchSummary where
  parseJSON = withObject "MatchSummary" \o ->
    MatchSummary <$> o .: "match_id"
                 <*> o .: "radiant_win"
                 <*> (posixSecondsToUTCTime <$> o .: "start_time")
                 <*> o .: "duration"
                 <*> o .: "lobby_type"
                 <*> o .: "game_mode"
                 <*> o .: "radiant_team"
                 <*> o .: "dire_team"

data Player = Player
  { profile :: Profile
  } deriving (Show, Eq, Ord)

instance FromJSON Player where
  parseJSON = withObject "Player" $ \o -> do
    Player <$> o .: "profile"

data Profile = Profile
  { accountID :: SteamID32
  , steamID :: SteamID64
  , personaName :: Text
  , profileUrl :: Text
  , avatarUrl :: Text
  , avatarFullUrl :: Text
  , avatarMediumUrl :: Text
  , plus :: Bool
  } deriving (Show, Eq, Ord)

instance FromJSON Profile where
  parseJSON = withObject "Profile" $ \o ->
    Profile <$> o .: "account_id"
            <*> o .: "steamid"
            <*> o .: "personaname"
            <*> o .: "profileurl"
            <*> o .: "avatar"
            <*> o .: "avatarfull"
            <*> o .: "avatarmedium"
            <*> o .: "plus"

data WinLoseRecord = WinLoseRecord
  { win :: Natural
  , lose :: Natural
  } deriving (Show, Eq, Ord)

instance FromJSON WinLoseRecord where
  parseJSON = withObject "WinLoseRecord" $ \o -> do
    WinLoseRecord <$> o .: "win"
                  <*> o .: "lose"

instance ToJSON WinLoseRecord where
  toJSON WinLoseRecord{..} =
    object [ "win" .= win, "lose" .= lose ]
  toEncoding WinLoseRecord{..} =
    pairs ("win" .= win <> "lose" .= lose)

data PlayerMatchSummary = PlayerMatchSummary
  { matchID :: MatchID
  , playerSlot :: PlayerSlot
  , radiantWin :: Bool
  , duration :: NominalDiffTime
  , gameMode :: GameModeID
  , lobbyType :: LobbyTypeID
  , heroID :: HeroID
  , startTime :: UTCTime
  , kills :: Natural
  , deaths :: Natural
  , assists :: Natural
  , averageRank :: Maybe Integer
  , partySize :: Maybe Natural
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON PlayerMatchSummary where
  parseJSON = withObject "PlayerMatchSummary" $ \o -> do
    PlayerMatchSummary <$> o .: "match_id"
                       <*> o .: "player_slot"
                       <*> o .: "radiant_win"
                       <*> o .: "duration"
                       <*> o .: "game_mode"
                       <*> o .: "lobby_type"
                       <*> o .: "hero_id"
                       <*> (posixSecondsToUTCTime <$> o .: "start_time")
                       <*> o .: "kills"
                       <*> o .: "deaths"
                       <*> o .: "assists"
                       <*> o .: "average_rank"
                       <*> o .: "party_size"

newtype Limit = Limit Integer
  deriving (Show, Read, Eq, Ord)

newtype Offset = Offset Integer
  deriving (Show, Read, Eq, Ord)

newtype HeroID = HeroID Int64
  deriving ( Show, Read, Eq, Ord
           , FromJSON, ToJSON, FromJSONKey, ToJSONKey
           , FromHttpApiData, ToHttpApiData
           )

data Hero = Hero
  { heroID :: HeroID
  , localizedName :: Text
  } deriving (Show, Eq, Ord)

instance FromJSON Hero where
  parseJSON = withObject "Hero" \o ->
    Hero <$> o .: "id"
         <*> o .: "localized_name"

newtype ItemID = ItemID Int64
  deriving ( Show, Read, Eq, Ord
           , FromJSON, ToJSON
           , FromJSONKey, ToJSONKey
           )

newtype ItemKey = ItemKey Text
  deriving ( Show, Read, Eq, Ord
           , FromJSON, ToJSON
           , FromJSONKey, ToJSONKey
           )

data Item = Item
  { itemID :: ItemID
  , dname :: Maybe Text
  , cost :: Maybe Integer
  , tier :: Maybe Integer
  } deriving (Show, Eq, Ord)

instance FromJSON Item where
  parseJSON = withObject "Item" \o -> do
    Item <$> o .: "id"
         <*> o .:? "dname"
         <*> o .:? "cost"
         <*> o .:? "tier"

newtype RegionID = RegionID Int64
  deriving (Show, Read, Eq, Ord)
  deriving (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
    via StringEncodedIntegral Int64

newtype Region = Region Text
  deriving ( Show, Read, Eq, Ord
           , FromJSON, ToJSON
           )

newtype GameModeID = GameModeID Int64
  deriving ( Show, Read, Eq, Ord
           , FromJSON, ToJSON, FromJSONKey, ToJSONKey
           , FromHttpApiData, ToHttpApiData
           )

data GameMode = GameMode
  { gameModeID :: GameModeID
  , name :: Text
  , balanced :: Maybe Bool
  } deriving (Show, Eq, Ord)

instance FromJSON GameMode where
  parseJSON = withObject "GameMode" \o ->
    GameMode <$> o .: "id"
             <*> o .: "name"
             <*> o .:? "balanced"

newtype LobbyTypeID = LobbyTypeID Int64
  deriving ( Show, Read, Eq, Ord
           , FromJSON, ToJSON, FromJSONKey, ToJSONKey
           , FromHttpApiData, ToHttpApiData
           )

data LobbyType = LobbyType
  { lobbyTypeID :: LobbyTypeID
  , name :: Text
  , balanced :: Maybe Bool
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON LobbyType where
  parseJSON = withObject "LobbyType" \o ->
    LobbyType <$> o .: "id"
              <*> o .: "name"
              <*> o .:? "balanced"
