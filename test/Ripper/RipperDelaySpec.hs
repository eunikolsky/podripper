module Ripper.RipperDelaySpec where

import RSSGen.Duration
import Ripper.RipperDelay
import Test.Hspec

spec :: Spec
spec = do
  describe "getRipperDelay" $ do
    it "returns default delay" $
      getRipperDelay `shouldBe` RetryDelay (durationMinutes 10)
