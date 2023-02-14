{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RSSFeedSpec where

import RSSFeed
import Types

import Control.Exception
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
  -- the real filesystem IO is used for testing `parseFeed` here because the
  -- implementation may or may not use a function like `eitherDecodeFileStrict`
  -- from `aeson`, which works in `IO`; using a custom monad for FS access,
  -- namely reading files, would force the implementation to read the file and
  -- parse JSON separately (which may or may not be fine)
  describe "parseFeed" $ do
    let dir = "test/sandbox"
        feedName = "foo"
        filename = dir </> feedName <> "_feed.json"

    it "parses RSSFeedConfig from file based on feed name" $ do
      ensureDirectory dir
      BS.writeFile filename validConfigString

      (feed, _) <- parseFeed dir feedName
      isJust feed `shouldBe` True

    it "returns the feed filename" $ do
      ensureDirectory dir
      BS.writeFile filename validConfigString

      (_, actualFilename) <- parseFeed dir feedName
      actualFilename `shouldBe` filename

    it "returns an error when parsing fails" $ do
      ensureDirectory dir
      BS.writeFile filename BS.empty

      (error, _) <- parseFeed dir feedName
      isNothing error `shouldBe` True

ensureDirectory :: FilePath -> IO ()
ensureDirectory dir = catchJust
  (\e -> if isAlreadyExistsError e then Just () else Nothing)
  (createDirectory dir)
  (const $ pure ())
