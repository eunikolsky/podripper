module Ripper.RipperDelaySpec where

import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Data.Time.TZInfo
import Data.Time.TZTime
import RSSGen.Duration
import Ripper.RipperDelay
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "getRipperDelay" $ do
    context "after ripping ended" $ do
      prop "returns 1 s within 5 minutes" $ \(Now now) -> do
        offset <- realToFrac <$> choose @Float (0, 5 * 60)
        let ripEndTime = addTime (negate offset) now
        pure $ getRipperDelay (Just ripEndTime) now == RetryDelay (durationSeconds 1)

      prop "returns 3 s within 15 minutes" $ \(Now now) -> do
        offset <- realToFrac <$> choose @Float ((5 * 60) + 1, 15 * 60)
        let ripEndTime = addTime (negate offset) now
        pure $ getRipperDelay (Just ripEndTime) now == RetryDelay (durationSeconds 3)

    context "without previous ripping" $
      prop "returns default delay" $ \(Now now) ->
        getRipperDelay Nothing now == RetryDelay (durationMinutes 10)

newtype Now = Now TZTime
  deriving newtype (Show)

instance Arbitrary Now where
  arbitrary = do
    day <- fromOrdinalDate
      <$> chooseInteger (2023, 2123)
      <*> chooseInt (1, 365)
    timeOfDay <- TimeOfDay
      <$> chooseInt (0, 23)
      <*> chooseInt (0, 59)
      <*> (realToFrac <$> chooseInt (0, 59))
    let time = LocalTime day timeOfDay
    let tzInfo = fromLabel Europe__Kyiv
    pure . Now $ fromLocalTime tzInfo time
