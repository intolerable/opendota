module OpenDota.TypesSpec where

import Data.Aeson
import Hedgehog
import Test.Hspec
import Test.Hspec.Hedgehog

import qualified OpenDota.Hedgehog as Gen

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "JSON roundtrips" do

    it "roundtrips MatchID" do
      jsonRoundtrips Gen.matchID

    it "roundtrips SteamID32" do
      jsonRoundtrips Gen.steamID32

    it "roundtrips SteamID64" do
      jsonRoundtrips Gen.steamID64

    it "roundtrips HeroID" do
      jsonRoundtrips Gen.heroID

    it "roundtrips ItemID" do
      jsonRoundtrips Gen.itemID

    it "roundtrips LobbyTypeID" do
      jsonRoundtrips Gen.lobbyTypeID

    it "roundtrips GameModeID" do
      jsonRoundtrips Gen.gameModeID

    it "roundtrips PlayerSlot" do
      jsonRoundtrips Gen.playerSlot

    it "roundtrips PlayerColor" do
      jsonRoundtrips Gen.playerColor

    it "roundtrips WinLoseRecord" do
      jsonRoundtrips Gen.winLoseRecord

jsonRoundtrips :: (Show a, Eq a, FromJSON a, ToJSON a)
               => Gen a -> PropertyT IO ()
jsonRoundtrips gen = do
  v <- forAll gen
  tripping v toJSON fromJSON
