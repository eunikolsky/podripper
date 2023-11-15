{-# LANGUAGE QuasiQuotes #-}

module RipConfigSpec where

import Data.Aeson
import RSSGen.Duration
import RipConfig
import Test.Hspec
import Text.RawString.QQ
import Data.Text.Lazy.Encoding (encodeUtf8)

spec :: Spec
spec = do
  describe "RipConfig" $ do
    describe "FromJSON" $ do
      it "parses json with all fields" $ do
        let s = encodeUtf8 [r|{
          "streamURL": "http://example.org",
          "duration": "4m",
          "retryDelay": "8s",
          "ripDirName": "test",
          "podArtist": "Хакер",
          "podAlbum": "Hackers"
        }|]

        let expected = RipConfig
              { streamURL = "http://example.org"
              , duration = durationMinutes 4
              , retryDelay = RetryDelay $ Duration 8
              , ripDirName = "test"
              , podArtist = "Хакер"
              , podAlbum = "Hackers"
              }

        eitherDecode' s `shouldBe` Right expected

      it "ignores unknown fields" $ do
        let s = encodeUtf8 [r|{
          "streamURL": "http://example.org",
          "duration": "4s",
          "_comment_duration": "just 4",
          "retryDelay": "2m",
          "ripDirName": "test",
          "podArtist": "Хакер",
          "podAlbum": "Hackers",
          "?": "?"
        }|]

        let expected = RipConfig
              { streamURL = "http://example.org"
              , duration = Duration 4
              , retryDelay = RetryDelay $ durationMinutes 2
              , ripDirName = "test"
              , podArtist = "Хакер"
              , podAlbum = "Hackers"
              }

        eitherDecode' s `shouldBe` Right expected
