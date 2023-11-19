{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module Ripper.RipperDelaySpec where

import Data.Maybe
import Data.Time hiding (utc)
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
        pure $ getRipperDelay [interval] (Just ripEndTime) now == RetryDelay (durationSeconds 1)

      prop "returns 3 s within 15 minutes" $ \interval (Now now) -> do
        offset <- realToFrac <$> choose @Float ((5 * 60) + 1, 15 * 60)
        let ripEndTime = addTime (negate offset) now
        pure $ getRipperDelay [interval] (Just ripEndTime) now == RetryDelay (durationSeconds 3)

      it "returns default delay when outside of any time interval" $ do
        let ripEndTime = [tz|2023-11-19 04:00:00 [Europe/Kyiv]|] -- Sunday
            now = addTime (minutes 15 + seconds 1) ripEndTime
        getRipperDelay testIntervals (Just ripEndTime) now `shouldBe` defaultDelay

      it "returns fixed delay when inside first time interval" $ do
        let ripEndTime = [tz|2023-11-16 04:00:00 [Europe/Kyiv]|] -- Thursday
            now = addTime (minutes 15 + seconds 1) ripEndTime
        getRipperDelay testIntervals Nothing now `shouldBe` intervalDelay

      it "returns fixed delay when inside second time interval" $ do
        let ripEndTime = [tz|2023-11-18 22:00:00 [Europe/Kyiv]|] -- Saturday
            now = addTime (minutes 15 + seconds 1) ripEndTime
        getRipperDelay testIntervals Nothing now `shouldBe` intervalDelay

      it "returns default delay when there are no time intervals" $ do
        let ripEndTime = [tz|2023-11-19 04:00:00 [Europe/Kyiv]|] -- Sunday
            now = addTime (minutes 15 + seconds 1) ripEndTime
        getRipperDelay [] (Just ripEndTime) now `shouldBe` defaultDelay

    context "without previous ripping" $ do
      it "returns default delay when outside time interval" $ do
        let now = [tz|2023-11-19 04:00:00 [Europe/Kyiv]|] -- Sunday
        getRipperDelay testIntervals Nothing now `shouldBe` defaultDelay

      it "returns fixed delay when inside time interval" $ do
        let now = [tz|2023-11-16 04:00:00 [Europe/Kyiv]|] -- Thursday
        getRipperDelay testIntervals Nothing now `shouldBe` intervalDelay

newYork :: TZInfo
newYork = fromLabel America__New_York

testInterval0, testInterval1 :: RipperInterval
testInterval0 = fromJust $ mkRipperInterval Wednesday (read "20:00:00", read "22:00:00") newYork
testInterval1 = fromJust $ mkRipperInterval Saturday (read "19:00:00", read "21:00:00") utc

testIntervals :: [RipperInterval]
testIntervals = [testInterval0, testInterval1]

defaultDelay, intervalDelay :: RetryDelay
defaultDelay = RetryDelay (durationMinutes 10)
intervalDelay = RetryDelay (durationSeconds 20)

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
