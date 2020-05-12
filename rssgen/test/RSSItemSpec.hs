{-# LANGUAGE OverloadedStrings #-}

module RSSItemSpec where

--import Control.Monad.IO.Class
--import qualified Data.Text.IO as TIO
import Data.Time.Calendar
import Data.Time.Clock

import RSSItem
import UpstreamRSSFeed

import Test.Hspec

spec :: Spec
spec =
  describe "closestUpstreamItemToTime" $ do
    it "returns the closest item by time" $ do
      let item0 = UpstreamRSSItem "item0" $ utcTime 2020 01 01 10 15 00
          item1 = UpstreamRSSItem "item1" $ utcTime 2020 01 04 10 15 00
          time = utcTime 2020 01 03 12 00 00
      closestUpstreamItemToTime [item0, item1] time `shouldBe` Just item1

    it "returns Nothing when closest item is more than one day away" $ do
      let item0 = UpstreamRSSItem "item0" $ utcTime 2020 01 01 00 00 00
          item1 = UpstreamRSSItem "item1" $ utcTime 2020 01 04 00 00 00
          time = utcTime 2020 01 02 23 59 59
      closestUpstreamItemToTime [item0, item1] time `shouldBe` Nothing

    it "returns Nothing when there are no items" $ do
      let time = utcTime 2020 01 03 12 00 00
      closestUpstreamItemToTime [] time `shouldBe` Nothing

  where
    utcTime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
    utcTime year month day hour minute second = UTCTime
      (fromGregorian year month day)
      (secondsToDiffTime . fromIntegral $ second + (minute * 60) + (hour * 60 * 60))
