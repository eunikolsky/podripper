{-# LANGUAGE OverloadedStrings #-}

module RSSGen.DatabaseSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Calendar
import Data.Time.Clock

import RSSGen.Database
import RSSGen.Types
import RSSGen.UpstreamRSSFeed

import Test.Hspec

podcastId = "radiot" :: PodcastId

spec :: Spec
spec =
  describe "closestUpstreamItemToTime" $ do
    it "returns the closest item by time within one day" $ do
      let item0 = UpstreamRSSItem "item0" (utcTime 2020 01 01 10 15 00) "" "" podcastId
          item1 = UpstreamRSSItem "item1" (utcTime 2020 01 04 10 15 00) "" "" podcastId
          time = utcTime 2020 01 03 12 00 00

      conn <- liftIO $ openDatabase InMemory
      saveUpstreamRSSItems conn [item0, item1]
      actual <- closestUpstreamItemToTime (Hours 24) podcastId conn time
      liftIO $ closeDatabase conn
      actual `shouldBe` Just item1

    it "returns Nothing when closest item is more than one day away" $ do
      let item0 = UpstreamRSSItem "item0" (utcTime 2020 01 01 00 00 00) "" "" podcastId
          item1 = UpstreamRSSItem "item1" (utcTime 2020 01 04 00 00 00) "" "" podcastId
          time = utcTime 2020 01 02 23 59 59

      conn <- liftIO $ openDatabase InMemory
      saveUpstreamRSSItems conn [item0, item1]
      actual <- closestUpstreamItemToTime (Hours 24) podcastId conn time
      liftIO $ closeDatabase conn
      actual `shouldBe` Nothing

    it "returns Nothing when there are no items" $ do
      let time = utcTime 2020 01 03 12 00 00

      conn <- liftIO $ openDatabase InMemory
      actual <- closestUpstreamItemToTime (Hours 24) podcastId conn time
      liftIO $ closeDatabase conn
      actual `shouldBe` Nothing

    it "returns newest item when there are multiple items within one day" $ do
      let newer = UpstreamRSSItem "newer" (utcTime 2021 08 14 01 14 30) "" "" podcastId
          older = UpstreamRSSItem "older" (utcTime 2021 08 13 23 04 06) "" "" podcastId
          time = utcTime 2021 08 13 22 47 09

      conn <- liftIO $ openDatabase InMemory
      saveUpstreamRSSItems conn [newer, older]
      actual <- closestUpstreamItemToTime (Hours 24) podcastId conn time
      liftIO $ closeDatabase conn
      actual `shouldBe` Just newer

    it "returns newest item when there are multiple items within specified interval" $ do
      let tooNew = UpstreamRSSItem "tooNew" (utcTime 2021 08 14 01 14 30) "" "" podcastId
          newer = UpstreamRSSItem "newer" (utcTime 2021 08 14 00 14 30) "" "" podcastId
          older = UpstreamRSSItem "older" (utcTime 2021 08 13 23 04 06) "" "" podcastId
          time = utcTime 2021 08 13 22 47 09

      conn <- liftIO $ openDatabase InMemory
      saveUpstreamRSSItems conn [tooNew, newer, older]
      actual <- closestUpstreamItemToTime (Hours 2) podcastId conn time
      liftIO $ closeDatabase conn
      actual `shouldBe` Just newer

    it "returns the closest item within specified interval in the past" $ do
      let item0 = UpstreamRSSItem "item0" (utcTime 2023 01 04 02 01 00) "" "" podcastId
          item1 = UpstreamRSSItem "item1" (utcTime 2023 01 03 01 01 00) "" "" podcastId
          time = utcTime 2023 01 03 13 00 00

      conn <- liftIO $ openDatabase InMemory
      saveUpstreamRSSItems conn [item0, item1]
      actual <- closestUpstreamItemToTime (Hours 12) podcastId conn time
      liftIO $ closeDatabase conn
      actual `shouldBe` Just item1

  where
    utcTime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
    utcTime year month day hour minute second = UTCTime
      (fromGregorian year month day)
      (secondsToDiffTime . fromIntegral $ second + (minute * 60) + (hour * 60 * 60))
