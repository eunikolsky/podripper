{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RSSFeedSpec where

import RSSFeed
import Types

import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.String
import System.Directory
import System.FilePath
import System.IO.Error
import Test.Hspec
import Text.RawString.QQ
import qualified Data.ByteString as BS

validConfigString :: IsString s => s
validConfigString = [r|{
  "title": "foo", "description": "bar", "language": "en",
  "podcastLink": "podcast.link", "imageLink": "image.link",
  "selfLink": "self.link", "upstreamRSSURL": "url",
  "closestUpstreamItemIntervalHours": 8
  }|]

spec :: Spec
spec = do
  describe "RSSFeedConfig" $
    describe "FromJSON" $ do
      it "parses JSON with all fields" $ do
        let expected = RSSFeedConfig
              { title = "foo"
              , description = "bar"
              , language = "en"
              , podcastLink = "podcast.link"
              , imageLink = "image.link"
              , selfLink = "self.link"
              , upstreamRSSURL = Just "url"
              , closestUpstreamItemInterval = Hours 8
              }
        eitherDecode' validConfigString `shouldBe` Right expected

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

  -- warning: currently not `parallel`-safe!
  -- the real filesystem IO is used for testing `parseFeedConfig` here because the
  -- implementation may or may not use a function like `eitherDecodeFileStrict`
  -- from `aeson`, which works in `IO`; using a custom monad for FS access,
  -- namely reading files, would force the implementation to read the file and
  -- parse JSON separately (which may or may not be fine)
  describe "parseFeedConfig" $ do
    let dir = "test/sandbox"
        feedName = "foo"
        filename = dir </> feedName <> "_feed.json"

    describe "no overlay file" $ do
      it "parses RSSFeedConfig from file based on feed name" $ do
        ensureEmptyDirectory dir
        BS.writeFile filename validConfigString

        (config, _) <- parseFeedConfig dir feedName
        isJust config `shouldBe` True

      it "returns the feed filename" $ do
        ensureEmptyDirectory dir
        BS.writeFile filename validConfigString

        (_, actualFilenames) <- parseFeedConfig dir feedName
        actualFilenames `shouldBe` [filename]

      it "returns an error when parsing fails" $ do
        ensureEmptyDirectory dir
        BS.writeFile filename BS.empty

        (config, _) <- parseFeedConfig dir feedName
        config `shouldBe` Nothing

    describe "overlay file support" $ do
      let overlayFilename = dir </> feedName <> "_feed_overlay.json"

      it "overwrites fields from overlay file" $ do
        ensureEmptyDirectory dir
        BS.writeFile filename validConfigString
        BS.writeFile overlayFilename [r|{"podcastLink": "overwrite", "imageLink": "newImage"}|]

        let expected = RSSFeedConfig
              { title = "foo"
              , description = "bar"
              , language = "en"
              , podcastLink = "overwrite"
              , imageLink = "newImage"
              , selfLink = "self.link"
              , upstreamRSSURL = Just "url"
              , closestUpstreamItemInterval = Hours 8
              }

        (config, _) <- parseFeedConfig dir feedName
        config `shouldBe` Just expected

      it "returns the feed and overlay filenames" $ do
        ensureEmptyDirectory dir
        BS.writeFile filename validConfigString
        BS.writeFile overlayFilename [r|{"podcastLink": "overwrite", "imageLink": "newImage"}|]

        (_, actualFilenames) <- parseFeedConfig dir feedName
        actualFilenames `shouldMatchList` [filename, overlayFilename]

ensureEmptyDirectory :: FilePath -> IO ()
ensureEmptyDirectory dir = do
  exists <- doesDirectoryExist dir
  when exists $ removeDirectoryRecursive dir
  createDirectory dir
