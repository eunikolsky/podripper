{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RSSFeedSpec where

import RSSFeed
import Types

import Data.Aeson
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "RSSFeedConfig" $
    describe "FromJSON" $ do
      it "parses JSON with all fields" $ do
        let text = [r|{
        "title": "foo", "description": "bar", "language": "en",
        "podcastLink": "podcast.link", "imageLink": "image.link",
        "selfLink": "self.link", "upstreamRSSURL": "url",
        "closestUpstreamItemIntervalHours": 8
        }|]
            expected = RSSFeedConfig
              { title = "foo"
              , description = "bar"
              , language = "en"
              , podcastLink = "podcast.link"
              , imageLink = "image.link"
              , selfLink = "self.link"
              , upstreamRSSURL = Just "url"
              , closestUpstreamItemInterval = Hours 8
              }
        eitherDecode' text `shouldBe` Right expected

      it "parses JSON without upstreamRSSURL" $ do
        let text = [r|{
        "title": "foo", "description": "bar", "language": "en",
        "podcastLink": "podcast.link", "imageLink": "image.link",
        "selfLink": "self.link", "closestUpstreamItemIntervalHours": 24
        }|]
            expected = RSSFeedConfig
              { title = "foo"
              , description = "bar"
              , language = "en"
              , podcastLink = "podcast.link"
              , imageLink = "image.link"
              , selfLink = "self.link"
              , upstreamRSSURL = Nothing
              , closestUpstreamItemInterval = Hours 24
              }
        eitherDecode' text `shouldBe` Right expected
