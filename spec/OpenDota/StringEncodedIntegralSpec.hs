module OpenDota.StringEncodedIntegralSpec where

import Control.Monad
import Data.Aeson
import Data.Int
import Data.Text (Text)
import Test.Hspec

import OpenDota.StringEncodedIntegral

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "StringEncodedIntegral" do

    describe "FromJSON" do

      it "parses string-encoded Int64s from JSON" do

        let examples :: [(Text, Int64)]
            examples =
              [ ("0", 0)
              , ("-1", -1)
              ]
        forM_ examples \(s, i) ->
          fmap fromStringEncodedIntegral (fromJSON (String s))
            `shouldBe` pure i
