module RSSGen.DurationSpec where

import Data.Either
import RSSGen.Duration
import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec =
  describe "parseDuration" $ do
    it "parses duration in seconds" $ do
      parseDuration "5s" `shouldParse` Duration 5
      parseDuration "3600s" `shouldParse` Duration 3600

    it "parses duration in minutes" $ do
      parseDuration "1m" `shouldParse` Duration 60
      parseDuration "120m" `shouldParse` Duration (120 * 60)

    it "parses duration in hours" $ do
      parseDuration "1h" `shouldParse` Duration (60 * 60)
      parseDuration "26h" `shouldParse` Duration (26 * 60 * 60)

    it "fails to parse empty input" $
      parseDuration "" `shouldSatisfy` isLeft

    it "fails to parse space between number and unit" $
      parseDuration "42 s" `shouldSatisfy` isLeft

    it "fails to parse fractional number" $
      parseDuration "3.5h" `shouldSatisfy` isLeft

    it "fails to parse number without unit" $
      parseDuration "42" `shouldSatisfy` isLeft

    it "doesn't accept anything after the unit" $
      parseDuration "42min" `shouldSatisfy` isLeft
