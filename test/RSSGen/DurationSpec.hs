module RSSGen.DurationSpec where

import RSSGen.Duration
import Test.Hspec

spec :: Spec
spec =
  describe "parseDuration" $ do
    it "fails to parse empty input" $
      parseDuration "" `shouldBe` Nothing
