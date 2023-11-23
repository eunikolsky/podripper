{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module Ripper.RipperDelaySpec where

import Data.Either
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
            --         =     2023-11-18 21:00:00 America/New_York, Saturday
            --         =     2023-11-19 02:00:00 UTC, Sunday
            now = addTime (minutes 15 + seconds 1) ripEndTime
        getRipperDelay testIntervals (Just ripEndTime) now `shouldBe` defaultDelay

      it "returns fixed delay when inside first time interval" $ do
        let ripEndTime = [tz|2023-11-16 04:00:00 [Europe/Kyiv]|] -- Thursday
            --         =     2023-11-15 21:00:00 America/New_York, Wednesday
            --         =     2023-11-16 02:00:00 UTC, Thursday
            now = addTime (minutes 15 + seconds 1) ripEndTime
        getRipperDelay testIntervals Nothing now `shouldBe` riDelay testInterval0

      it "returns fixed delay when inside second time interval" $ do
        let ripEndTime = [tz|2023-11-18 22:00:00 [Europe/Kyiv]|] -- Saturday
            --         =     2023-11-18 15:00:00 America/New_York, Saturday
            --         =     2023-11-18 20:00:00 UTC, Saturday
            now = addTime (minutes 15 + seconds 1) ripEndTime
        getRipperDelay testIntervals Nothing now `shouldBe` riDelay testInterval1

      it "returns default delay when there are no time intervals" $ do
        let ripEndTime = [tz|2023-11-19 04:00:00 [Europe/Kyiv]|] -- Sunday
            now = addTime (minutes 15 + seconds 1) ripEndTime
        getRipperDelay [] (Just ripEndTime) now `shouldBe` defaultDelay

    context "without previous ripping" $ do
      it "returns default delay when outside time interval" $ do
        let now = [tz|2023-11-19 04:00:00 [Europe/Kyiv]|] -- Sunday
            --  =     2023-11-18 21:00:00 America/New_York, Saturday
            --  =     2023-11-19 02:00:00 UTC, Sunday
        getRipperDelay testIntervals Nothing now `shouldBe` defaultDelay

      it "returns fixed delay when inside time interval" $ do
        let now = [tz|2023-11-16 04:00:00 [Europe/Kyiv]|] -- Thursday
            --  =     2023-11-15 21:00:00 America/New_York, Wednesday
            --  =     2023-11-16 02:00:00 UTC, Thursday
        getRipperDelay testIntervals Nothing now `shouldBe` riDelay testInterval0

      it "doesn't delay by more than to the next time interval" $ do
        let closeToTestInterval0 = fromJust $ mkRipperInterval Wednesday (read "17:00:00", read "19:00:00") newYork (RetryDelay $ durationMinutes 22)
        -- just before the interval's start
        let now = [tz|2023-11-15 23:58:40 [Europe/Kyiv]|] -- Wednesday
            --  =     2023-11-15 16:58:40 America/New_York, Wednesday
            --  =     2023-11-15 21:58:40 UTC, Wednesday
            expected = RetryDelay $ durationSeconds $ 60 + 20 + 1
            -- ^ 16:58:40 to 17:00:00 + extra second to get into the interval
        getRipperDelay [testInterval0, closeToTestInterval0] Nothing now `shouldBe` expected

      it "doesn't consider today's interval before now" $ do
        let intervalDelay = RetryDelay $ durationMinutes 22
            closeToTestInterval0 = fromJust $ mkRipperInterval Wednesday (read "17:00:00", read "19:00:00") newYork intervalDelay
        -- just after the interval's start
        let now = [tz|2023-11-16 00:00:40 [Europe/Kyiv]|] -- Thursday
            --  =     2023-11-15 17:00:40 America/New_York, Wednesday
            --  =     2023-11-15 22:00:40 UTC, Wednesday
        getRipperDelay [testInterval0, closeToTestInterval0] Nothing now `shouldBe` intervalDelay

  describe "parseRipperInterval" $ do
    it "parses an interval in UTC" $ do
      actual <- parseRipperInterval "Mo 11:01-13:45 UTC: 8s"
      let expected = mkRipperInterval' Monday (read "11:01:00", read "13:45:00") utc (RetryDelay $ durationSeconds 8)
      actual `shouldBe` expected

    it "parses an interval in a timezone" $ do
      actual <- parseRipperInterval "Su 12:59-23:48 America/New_York: 9m"
      let expected = mkRipperInterval' Sunday (read "12:59:00", read "23:48:00") (fromLabel America__New_York) (RetryDelay $ durationMinutes 9)
      actual `shouldBe` expected

    it "fails on an unknown timezone" $ do
      actual <- parseRipperInterval "Su 12:59-23:48 Unknown: 9m"
      actual `shouldSatisfy` isLeft

    it "fails on an unordered interval" $ do
      actual <- parseRipperInterval "Su 14:00-12:00 UTC: 9m"
      actual `shouldSatisfy` isLeft

mkRipperInterval' :: DayOfWeek -> (TimeOfDay, TimeOfDay) -> TZInfo -> RetryDelay -> Either a RipperInterval
mkRipperInterval' d ti tz' = Right . fromJust . mkRipperInterval d ti tz'

newYork :: TZInfo
newYork = fromLabel America__New_York

testInterval0, testInterval1 :: RipperInterval
testInterval0 = fromJust $ mkRipperInterval Wednesday (read "20:00:00", read "22:00:00") newYork (RetryDelay $ durationMinutes 8)
testInterval1 = fromJust $ mkRipperInterval Saturday (read "19:00:00", read "21:00:00") utc (RetryDelay $ durationHours 1)

testIntervals :: [RipperInterval]
testIntervals = [testInterval0, testInterval1]

defaultDelay :: RetryDelay
defaultDelay = RetryDelay (durationMinutes 10)

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
    pure . fromJust $ mkRipperInterval day (from', to') (fromLabel America__Anchorage) (RetryDelay $ durationSeconds 20)
