module OpenDotaSpec where

-- import Control.Monad.IO.Class
import Network.HTTP.Client.TLS
import Servant.Client
import Test.Hspec

import OpenDota.API
import OpenDota.Types (SteamID32(..), MatchID(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll newClientEnv do

  describe "/constants" do

    describe "/heroes" do

      it "gets the hero data" \cenv -> do
        assertClientSucceeds $
          runClientM (openDotaClient // constants // heroes) cenv

    describe "/items" do
      it "gets the item data" \cenv -> do
        assertClientSucceeds $
          runClientM (openDotaClient // constants // items) cenv

    describe "/region" do
      it "gets the region data" \cenv -> do
        assertClientSucceeds $
          runClientM (openDotaClient // constants // regions) cenv

    describe "/game_mode" do
      it "gets the game mode data" \cenv -> do
        assertClientSucceeds $
          runClientM (openDotaClient // constants // gameModes) cenv

    describe "/lobby_type" do
      it "gets the lobby type data" \cenv -> do
        assertClientSucceeds $
          runClientM (openDotaClient // constants // lobbyTypes) cenv

    describe "/player_color" do
      it "gets the player color data" \cenv -> do
        assertClientSucceeds $
          runClientM (openDotaClient // constants // playerColors) cenv

  describe "/players" do

    describe "/{player_id}" do

      describe "/wl" do
        it "gets a win-loss record" \cenv -> do
          assertClientSucceeds $
            runClientM (openDotaClient // players /: testSteamID32 // wl) cenv

      describe "/recentMatches" do
        it "retrieves recent match summaries" \cenv -> do
          assertClientSucceeds $
            runClientM (openDotaClient // players /: testSteamID32 // recentMatches) cenv

      describe "/matches" do
        it "retrieves the game summaries" \cenv -> do
          let opts = defaultPlayerMatchOptions
                { limit = Just 10 }
          assertClientSucceeds $
            runClientM (openDotaClient // players /: testSteamID32 // playerMatches /: opts) cenv

  describe "/matches" do

    it "retrieves a match" \cenv -> do
      assertClientSucceeds $
        runClientM (openDotaClient // matches /: testMatchID) cenv

  describe "/publicMatches" do

    it "retrieves a list of randomly sampled public matches" \cenv ->
      assertClientSucceeds $ do
        runClientM (openDotaClient // publicMatches /: Nothing) cenv

ensureClientSuccess :: IO (Either ClientError a) -> IO a
ensureClientSuccess act = do
  act >>= \case
    Left err -> do
      expectationFailure $ "Expected client to succeed, but got a ClientError: " <> show err
      error "ClientError"
    Right res -> pure res

assertClientSucceeds :: IO (Either ClientError a) -> Expectation
assertClientSucceeds act = do
  _ <- ensureClientSuccess act
  pure ()

testSteamID32 :: SteamID32
testSteamID32 = SteamID32 70255291

testMatchID :: MatchID
testMatchID = MatchID 7718901919

newClientEnv :: IO ClientEnv
newClientEnv =
  mkClientEnv <$> newTlsManager <*> pure openDotaBaseUrl
