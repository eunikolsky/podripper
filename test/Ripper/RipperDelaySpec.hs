{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module Ripper.RipperDelaySpec where

import Data.Maybe
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.TZInfo
import Data.Time.TZTime
import Data.Time.TZTime.QQ
import RSSGen.Duration
import Ripper.RipperDelay
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "getRipperDelay" $ do
    context "after ripping ended" $ do
      prop "returns 1 s within 5 minutes" $ \interval (Now now) -> do
        offset <- realToFrac <$> choose @Float (0, 5 * 60)
        let ripEndTime = addTime (negate offset) now
        pure $ getRipperDelay interval (Just ripEndTime) now == RetryDelay (durationSeconds 1)

      prop "returns 3 s within 15 minutes" $ \interval (Now now) -> do
        offset <- realToFrac <$> choose @Float ((5 * 60) + 1, 15 * 60)
        let ripEndTime = addTime (negate offset) now
        pure $ getRipperDelay interval (Just ripEndTime) now == RetryDelay (durationSeconds 3)

    context "without previous ripping" $ do
      let newYork = fromLabel America__New_York
      let interval = fromJust $ mkRipperInterval Wednesday (read "20:00:00", read "22:00:00") newYork

      it "returns default delay when outside time interval" $ do
        let now = [tz|2023-11-19 04:00:00 [Europe/Kyiv]|] -- Sunday
        getRipperDelay interval Nothing now `shouldBe` RetryDelay (durationMinutes 10)

      it "returns fixed delay when inside time interval" $ do
        let now = [tz|2023-11-16 04:00:00 [Europe/Kyiv]|] -- Thursday
        getRipperDelay interval Nothing now `shouldBe` RetryDelay (durationSeconds 20)

instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay
    <$> chooseInt (0, 23)
    <*> chooseInt (0, 59)
    <*> (realToFrac <$> chooseInt (0, 59))

newtype Now = Now TZTime
  deriving newtype (Show)

instance Arbitrary Now where
  arbitrary = do
    day <- fromOrdinalDate
      <$> chooseInteger (2023, 2123)
      <*> chooseInt (1, 365)
    timeOfDay <- arbitrary
    let time = LocalTime day timeOfDay
    let tzInfo = fromLabel Europe__Kyiv
    pure . Now $ fromLocalTime tzInfo time

instance Arbitrary RipperInterval where
  arbitrary = do
    day <- chooseEnum (Monday, Sunday)
    from <- arbitrary
    to <- arbitrary
    let from' = min from to
        to' = max from to
    pure . fromJust $ mkRipperInterval day (from', to') $ fromLabel America__Anchorage
