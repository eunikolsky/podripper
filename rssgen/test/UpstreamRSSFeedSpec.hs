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
          [ UpstreamRSSItem
              "Радио-Т 699"
              (utcTime 2020 04 25 18 01 56 tzEST)
              "<p><img src=\"https://radio-t.com/images/radio-t/rt699.jpg\" alt=\"\"></p>\n<p><em>Темы</em><ul>\n<li><a href=\"https://www.opennet.ru/opennews/art.shtml?num=52791\">Релиз Ubuntu 20.04 LTS</a> - <em>00:02:17</em>.</li>\n<li>…</li>\n<li><a href=\"https://radio-t.com/p/2020/04/21/prep-699/\">Темы слушателей</a> - <em>01:42:17</em>.</li>\n</ul></p>\n<audio src=\"https://cdn.radio-t.com/rt_podcast699.mp3\" preload=\"none\"></audio>"
          , UpstreamRSSItem
              "Радио-Т 698"
              (utcTime 2020 04 18 17 59 24 tzEST)
              "<p><img src=\"https://radio-t.com/images/radio-t/rt698.jpg\" alt=\"\"></p>\n<p><em>Темы</em><ul>\n<li><a href=\"https://www.engadget.com/github-core-features-free-211014706.html\">GitHub для групп теперь бесплатно</a> - <em>00:02:53</em>.</li>\n<li>…</li>\n<li><a href=\"https://radio-t.com/p/2020/04/14/prep-698/\">Темы слушателей</a> - <em>01:38:52</em>.</li>\n</ul></p>\n<audio src=\"https://cdn.radio-t.com/rt_podcast698.mp3\" preload=\"none\"></audio>"
          ]

        utcTime :: Integer -> Int -> Int -> Int -> Int -> Int -> Int -> UTCTime
        utcTime year month day hour minute second tzHourOffset = UTCTime
          (fromGregorian year month day)
          (secondsToDiffTime . fromIntegral $ second + (minute * 60) + ((hour - tzHourOffset) * 60 * 60))

        tzEST = -05
