module RSSGen.DurationSpec where

import Data.Either
import RSSGen.Duration
import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec =
  describe "parseDuration" $ do
    it "parses duration in seconds" $ do
      parseDuration "5s" `shouldParse` 5
      parseDuration "3600s" `shouldParse` 3600

    it "parses duration in minutes" $ do
      parseDuration "1m" `shouldParse` 60
      parseDuration "120m" `shouldParse` (120 * 60)

    it "fails to parse empty input" $
      parseDuration "" `shouldSatisfy` isLeft
