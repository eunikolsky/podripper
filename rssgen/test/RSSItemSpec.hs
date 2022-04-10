module RSSItemSpec where

import Data.Time.Calendar
import Data.Time.LocalTime

import RSSItem

import Test.Hspec

-- | @ZonedTime@ with the @Eq@ instance. For @TimeZone@,
-- only the @timeZoneMinutes@ fields are compared.
newtype EqZonedTime = EqZonedTime ZonedTime
  deriving Show

instance Eq EqZonedTime where
  (EqZonedTime (ZonedTime time0 (TimeZone tzMinutes0 _ _)))
    == (EqZonedTime (ZonedTime time1 (TimeZone tzMinutes1 _ _)))
    = time0 == time1 && tzMinutes0 == tzMinutes1

spec :: Spec
spec =
  describe "localTimeToZonedTime" $ do
    -- TODO no DST context?
    -- TODO get timezone from local time?
    it "returns original time for no-DST local time when there is no DST" $ do
      let localTime = LocalTime (fromGregorian 2022 03 26) (TimeOfDay 21 50 18)
          noDST = hoursToTimeZone 2
      actual <- EqZonedTime . fst <$> localTimeToZonedTime' noDST localTime
      let expected = EqZonedTime $ ZonedTime localTime noDST
      actual `shouldBe` expected

    it "returns original time for no-DST local time when there is DST" $ do
      let localTime = LocalTime (fromGregorian 2022 03 26) (TimeOfDay 21 50 18)
          dst = hoursToTimeZone 3
      actual <- EqZonedTime . fst <$> localTimeToZonedTime' dst localTime
      let expected = EqZonedTime $ ZonedTime localTime (hoursToTimeZone 2)
      actual `shouldBe` expected

    it "returns original time for DST local time when there is DST" $ do
      let localTime = LocalTime (fromGregorian 2022 04 02) (TimeOfDay 22 50 18)
          dst = hoursToTimeZone 3
      actual <- EqZonedTime . fst <$> localTimeToZonedTime' dst localTime
      let expected = EqZonedTime $ ZonedTime localTime dst
      actual `shouldBe` expected

    it "returns original time for DST local time when there is no DST" $ do
      let localTime = LocalTime (fromGregorian 2021 10 30) (TimeOfDay 22 50 18)
          noDST = hoursToTimeZone 2
      actual <- EqZonedTime . fst <$> localTimeToZonedTime' noDST localTime
      let expected = EqZonedTime $ ZonedTime localTime (hoursToTimeZone 3)
      actual `shouldBe` expected
