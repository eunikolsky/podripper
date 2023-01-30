{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RSSFeedSpec where

import RSSFeed

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
        "selfLink": "self.link", "upstreamRSSURL": "url"
        }|]
            expected = RSSFeedConfig
              { title = "foo"
              , description = "bar"
              , language = "en"
              , podcastLink = "podcast.link"
              , imageLink = "image.link"
              , selfLink = "self.link"
              , upstreamRSSURL = Just "url"
              }
        eitherDecode' text `shouldBe` Right expected

      it "parses JSON without upstreamRSSURL" $ do
        let text = [r|{
        "title": "foo", "description": "bar", "language": "en",
        "podcastLink": "podcast.link", "imageLink": "image.link",
        "selfLink": "self.link"
        }|]
            expected = RSSFeedConfig
              { title = "foo"
              , description = "bar"
              , language = "en"
              , podcastLink = "podcast.link"
              , imageLink = "image.link"
              , selfLink = "self.link"
              , upstreamRSSURL = Nothing
              }
        eitherDecode' text `shouldBe` Right expected
