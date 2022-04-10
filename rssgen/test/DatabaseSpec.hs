{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Calendar
import Data.Time.Clock

import Database
import UpstreamRSSFeed

import Test.Hspec

podcastId = "radiot" :: PodcastId

spec :: Spec
spec =
  describe "closestUpstreamItemToTime" $ do
    it "returns the closest item by time" $ do
      let item0 = UpstreamRSSItem "item0" (utcTime 2020 01 01 10 15 00) "" "" podcastId
          item1 = UpstreamRSSItem "item1" (utcTime 2020 01 04 10 15 00) "" "" podcastId
          time = utcTime 2020 01 03 12 00 00

      conn <- liftIO $ openDatabase InMemory
      saveUpstreamRSSItems conn [item0, item1]
      actual <- closestUpstreamItemToTime podcastId conn time
      liftIO $ closeDatabase conn
      actual `shouldBe` Just item1

    it "returns Nothing when closest item is more than one day away" $ do
      let item0 = UpstreamRSSItem "item0" (utcTime 2020 01 01 00 00 00) "" "" podcastId
          item1 = UpstreamRSSItem "item1" (utcTime 2020 01 04 00 00 00) "" "" podcastId
          time = utcTime 2020 01 02 23 59 59

      conn <- liftIO $ openDatabase InMemory
      saveUpstreamRSSItems conn [item0, item1]
      actual <- closestUpstreamItemToTime podcastId conn time
      liftIO $ closeDatabase conn
      actual `shouldBe` Nothing

    it "returns Nothing when there are no items" $ do
      let time = utcTime 2020 01 03 12 00 00

      conn <- liftIO $ openDatabase InMemory
      actual <- closestUpstreamItemToTime podcastId conn time
      liftIO $ closeDatabase conn
      actual `shouldBe` Nothing

    xit "returns newest item when there are multiple items within one day" $ do
      let newer = UpstreamRSSItem "newer" (utcTime 2021 08 14 01 14 30) "" "" podcastId
          older = UpstreamRSSItem "older" (utcTime 2021 08 13 23 04 06) "" "" podcastId
          time = utcTime 2021 08 13 22 47 09

      conn <- liftIO $ openDatabase InMemory
      saveUpstreamRSSItems conn [newer, older]
      actual <- closestUpstreamItemToTime podcastId conn time
      liftIO $ closeDatabase conn
      actual `shouldBe` Just newer

  where
    utcTime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
    utcTime year month day hour minute second = UTCTime
      (fromGregorian year month day)
      (secondsToDiffTime . fromIntegral $ second + (minute * 60) + (hour * 60 * 60))
