module RSSGen.DurationSpec where

import Data.Either
import RSSGen.Duration
import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = describe "Duration" $ do
  describe "parseDuration" $ do
    it "parses duration in seconds" $ do
      parseDuration "5s" `shouldParse` durationSeconds 5
      parseDuration "3600s" `shouldParse` durationSeconds 3600

    it "parses duration in minutes" $ do
      parseDuration "1m" `shouldParse` durationMinutes 1
      parseDuration "120m" `shouldParse` durationMinutes 120

    it "parses duration in hours" $ do
      parseDuration "1h" `shouldParse` durationHours 1
      parseDuration "26h" `shouldParse` durationHours 26

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

  describe "isEmpty" $ do
    it "is true for zero durations" $ do
      isEmpty (durationSeconds 0) `shouldBe` True
      isEmpty (durationMinutes 0) `shouldBe` True
      isEmpty (durationHours 0) `shouldBe` True

    it "is false for non-zero durations" $ do
      isEmpty (durationSeconds 1) `shouldBe` False
      isEmpty (durationMinutes 1) `shouldBe` False
      isEmpty (durationHours 1) `shouldBe` False
