module OpenDota.Hedgehog where

import Data.Time.Clock
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import OpenDota.Types

matchID :: MonadGen m => m MatchID
matchID = MatchID <$> Gen.enumBounded

steamID32 :: MonadGen m => m SteamID32
steamID32 = SteamID32 <$> Gen.enumBounded

steamID64 :: MonadGen m => m SteamID64
steamID64 = SteamID64 <$> Gen.enumBounded

heroID :: MonadGen m => m HeroID
heroID = HeroID <$> Gen.enumBounded

itemID :: MonadGen m => m ItemID
itemID = ItemID <$> Gen.enumBounded

lobbyTypeID :: MonadGen m => m LobbyTypeID
lobbyTypeID = LobbyTypeID <$> Gen.enumBounded

gameModeID :: MonadGen m => m GameModeID
gameModeID = GameModeID <$> Gen.enumBounded

playerSlot :: MonadGen m => m PlayerSlot
playerSlot = PlayerSlot <$> faction <*> Gen.integral (Range.linear 0 4)

playerColor :: MonadGen m => m PlayerColor
playerColor =
  PlayerColor <$> Gen.enumBounded
              <*> Gen.enumBounded
              <*> Gen.enumBounded

winLoseRecord :: MonadGen m => m WinLoseRecord
winLoseRecord = do
  matchCount <- Gen.integral (Range.exponential 1 10240)
  winRate :: Double <- Gen.realFloat (Range.linearFracFrom 0.5 0.4 0.7)
  let w = floor $ fromIntegral matchCount * winRate
      l = matchCount - w
  pure $ WinLoseRecord w l

-- matchSummary :: MonadGen m => m MatchSummary
-- matchSummary = do
--   MatchSummary <$> matchID
--                <*> Gen.bool
--                <*> (posixSecondsToUtcTime <$> Gen.integral _) -- genStartTime
--                <*> undefined -- genDuration
--                <*> undefined -- lobbyType
--                <*> undefined -- gameMode
--                <*> undefined -- radiantTeam
--                <*> undefined -- direTeam
--   where
--     startDay = fromGregorian 2020 1 1
--     endDay = fromGregorian 2023 12 31
--     genStartTime = fmap posixSecondsToUtcTime $
--       Gen.integral (Range.linear
--     genDuration = undefined

faction :: MonadGen m => m Faction
faction = Gen.enumBounded
