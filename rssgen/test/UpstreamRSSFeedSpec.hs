{-# LANGUAGE OverloadedStrings #-}

module UpstreamRSSFeedSpec where

import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import Data.Time.Calendar
import Data.Time.Clock

import UpstreamRSSFeed

import Test.Hspec

spec :: Spec
spec =
  describe "parse" $
    it "parses the real valid RSS" $ do
      testRSS <- liftIO $ TIO.readFile "test/data/radiot.rss"
      parse testRSS `shouldBe` Just episodes

      where
        episodes :: [UpstreamRSSItem]
        episodes =
          [ UpstreamRSSItem "Радио-Т 699" $ utcTime 2020 04 25 18 01 56 tzEST
          , UpstreamRSSItem "Радио-Т 698" $ utcTime 2020 04 18 17 59 24 tzEST
          ]

        utcTime :: Integer -> Int -> Int -> Int -> Int -> Int -> Int -> UTCTime
        utcTime year month day hour minute second tzHourOffset = UTCTime
          (fromGregorian year month day)
          (secondsToDiffTime . fromIntegral $ second + (minute * 60) + ((hour - tzHourOffset) * 60 * 60))

        tzEST = -05
