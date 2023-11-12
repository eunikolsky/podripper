module RSSGen.DurationSpec where

import Data.Either
import RSSGen.Duration
import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec =
  describe "parseDuration" $ do
    it "parses number of seconds" $ do
      parseDuration "5s" `shouldParse` 5
      parseDuration "3600s" `shouldParse` 3600

    it "fails to parse empty input" $
      parseDuration "" `shouldSatisfy` isLeft
