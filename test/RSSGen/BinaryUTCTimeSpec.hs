{-# OPTIONS_GHC -Wno-orphans #-}

module RSSGen.BinaryUTCTimeSpec where

import Data.Binary
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import RSSGen.BinaryUTCTime ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Binary UTCRipTime instance" . modifyMaxSuccess (const 10) $
    prop "encoding roundtrip" $ \(time :: UTCTime) ->
      (decode . encode) time `shouldBe` time

instance Arbitrary UTCTime where
  arbitrary = do
    year <- chooseInteger (2023, 2123)
    dayOfYear <- chooseInt (1, 365)
    let day = fromOrdinalDate year dayOfYear
    dayTime <- realToFrac <$> choose @Double (0, 86400.9)
    pure $ UTCTime day dayTime
