module OpenDota.Types.PlayerSlot
  ( Faction(..)
  , PlayerSlot(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (prependFailure)
import Data.Int (Int32)
import Data.Bits
import Data.Functor.Contravariant

data Faction
  = Radiant
  | Dire
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

newtype PlayerSlotInt = PlayerSlotInt { unPlayerSlotInt :: Int32 }
  deriving ( Show, Eq, Ord
           , FromJSON, ToJSON
           )

playerSlotIntFactionBit :: Int
playerSlotIntFactionBit = 7

playerSlotIntFaction :: PlayerSlotInt -> Faction
playerSlotIntFaction (PlayerSlotInt i32) =
  if i32 `testBit` playerSlotIntFactionBit
    then Dire
    else Radiant

playerSlotIntTeamSlot :: PlayerSlotInt -> Int
playerSlotIntTeamSlot (PlayerSlotInt i32) =
  fromIntegral (i32 .&. 0b0111)

playerSlotFromInt :: PlayerSlotInt -> PlayerSlot
playerSlotFromInt p =
  PlayerSlot
    (playerSlotIntFaction p)
    (playerSlotIntTeamSlot p)

playerSlotToInt :: PlayerSlot -> PlayerSlotInt
playerSlotToInt (PlayerSlot f i) = PlayerSlotInt $
  case f of
    Radiant -> fromIntegral i
    Dire -> fromIntegral i `setBit` playerSlotIntFactionBit

data PlayerSlot = PlayerSlot Faction Int
  deriving (Show, Eq, Ord)

instance FromJSON PlayerSlot where
  parseJSON o = prependFailure "PlayerSlot: " do
    playerSlotFromInt <$> parseJSON o

instance ToJSON PlayerSlot where
  toJSON (PlayerSlot f i) = toJSON $
    case f of
      Radiant -> i
      Dire -> i `setBit` playerSlotIntFactionBit
  toEncoding (PlayerSlot f i) = toEncoding $
    case f of
      Radiant -> i
      Dire -> i `setBit` playerSlotIntFactionBit

instance FromJSONKey PlayerSlot where
  fromJSONKey = fmap (playerSlotFromInt . PlayerSlotInt) fromJSONKey

instance ToJSONKey PlayerSlot where
  toJSONKey = contramap (unPlayerSlotInt . playerSlotToInt) toJSONKey
