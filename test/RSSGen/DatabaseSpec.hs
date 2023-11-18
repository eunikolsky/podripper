{-# LANGUAGE OverloadedStrings #-}

module RSSGen.DatabaseSpec where

import Data.Time.Calendar
import Data.Time.Clock

import RSSGen.Database
import RSSGen.DownloaderTypes
import RSSGen.Duration
import RSSGen.UpstreamRSSFeed

import Test.Hspec

podcastId :: PodcastId
podcastId = "radiot"

spec :: Spec
spec = do
  describe "closestUpstreamItemToTime" $ do
    it "returns the closest item by time within one day" $ do
      let item0 = UpstreamRSSItem "item0" (utcTime 2020 01 01 10 15 00) "" "" podcastId
          item1 = UpstreamRSSItem "item1" (utcTime 2020 01 04 10 15 00) "" "" podcastId
          time = utcTime 2020 01 03 12 00 00

      actual <- withDB $ \conn -> do
        saveUpstreamRSSItems conn [item0, item1]
        closestUpstreamItemToTime (durationHours 24) podcastId conn time
      actual `shouldBe` Just item1

    it "returns Nothing when closest item is more than one day away" $ do
      let item0 = UpstreamRSSItem "item0" (utcTime 2020 01 01 00 00 00) "" "" podcastId
          item1 = UpstreamRSSItem "item1" (utcTime 2020 01 04 00 00 00) "" "" podcastId
          time = utcTime 2020 01 02 23 59 59

      actual <- withDB $ \conn -> do
        saveUpstreamRSSItems conn [item0, item1]
        closestUpstreamItemToTime (durationHours 24) podcastId conn time
      actual `shouldBe` Nothing

    it "returns Nothing when there are no items" $ do
      let time = utcTime 2020 01 03 12 00 00

      actual <- withDB $ \conn ->
        closestUpstreamItemToTime (durationHours 24) podcastId conn time
      actual `shouldBe` Nothing

    it "returns newest item when there are multiple items within one day" $ do
      let newer = UpstreamRSSItem "newer" (utcTime 2021 08 14 01 14 30) "" "" podcastId
          older = UpstreamRSSItem "older" (utcTime 2021 08 13 23 04 06) "" "" podcastId
          time = utcTime 2021 08 13 22 47 09

      actual <- withDB $ \conn -> do
        saveUpstreamRSSItems conn [newer, older]
        closestUpstreamItemToTime (durationHours 24) podcastId conn time
      actual `shouldBe` Just newer

    it "returns newest item when there are multiple items within specified interval" $ do
      let tooNew = UpstreamRSSItem "tooNew" (utcTime 2021 08 14 01 14 30) "" "" podcastId
          newer = UpstreamRSSItem "newer" (utcTime 2021 08 14 00 14 30) "" "" podcastId
          older = UpstreamRSSItem "older" (utcTime 2021 08 13 23 04 06) "" "" podcastId
          time = utcTime 2021 08 13 22 47 09

      actual <- withDB $ \conn -> do
        saveUpstreamRSSItems conn [tooNew, newer, older]
        closestUpstreamItemToTime (durationHours 2) podcastId conn time
      actual `shouldBe` Just newer

    it "returns the closest item within specified interval in the past" $ do
      let item0 = UpstreamRSSItem "item0" (utcTime 2023 01 04 02 01 00) "" "" podcastId
          item1 = UpstreamRSSItem "item1" (utcTime 2023 01 03 01 01 00) "" "" podcastId
          time = utcTime 2023 01 03 13 00 00

      actual <- withDB $ \conn -> do
        saveUpstreamRSSItems conn [item0, item1]
        closestUpstreamItemToTime (durationHours 12) podcastId conn time
      actual `shouldBe` Just item1

  describe "CacheItem" $ do
    let verifyPersistence item = do
          let url = "localhost"

          actual <- withDB $ \conn -> do
            setCacheItem conn url item
            getCacheItem conn url
          actual `shouldBe` Just item

    it "ETag can be persisted in the database" $ do
      verifyPersistence $ ETag "hello!"

    it "LastModified can be persisted in the database" $ do
      verifyPersistence $ LastModified "lastmod"

    it "ETag with LastModified can be persisted in the database" $ do
      verifyPersistence $ ETagWithLastModified "etag" "lastmod"

    it "Body can be persisted in the database" $ do
      verifyPersistence $ Body "some arbitrary response body"

utcTime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
utcTime year month day hour minute second = UTCTime
  (fromGregorian year month day)
  (secondsToDiffTime . fromIntegral $ second + (minute * 60) + (hour * 60 * 60))

withDB :: (DBConnection -> IO a) -> IO a
withDB = withDatabase InMemory
