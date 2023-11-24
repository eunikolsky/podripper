{-# LANGUAGE QuasiQuotes #-}

module RipConfigSpec where

import Data.Aeson
import Data.Time
import RSSGen.Duration
import RipConfig
import Ripper.RipperDelay
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
          "ripIntervals": [
            "Mo 13:00-15:00 America/Toronto: 2m"
          ],
          "ripDirName": "test",
          "podArtist": "Хакер",
          "podAlbum": "Hackers"
        }|]

        let expected = RipConfig
              { streamURL = "http://example.org"
              , duration = durationMinutes 4
              , retryDelay = RetryDelay $ durationSeconds 8
              , ripIntervalRefs =
                [ RipperIntervalRef Monday (read "13:00:00", read "15:00:00") "America/Toronto" (RetryDelay $ durationMinutes 2)
                ]
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
          "ripIntervals": [],
          "podArtist": "Хакер",
          "podAlbum": "Hackers",
          "?": "?"
        }|]

        let expected = RipConfig
              { streamURL = "http://example.org"
              , duration = durationSeconds 4
              , retryDelay = RetryDelay $ durationMinutes 2
              , ripIntervalRefs = []
              , ripDirName = "test"
              , podArtist = "Хакер"
              , podAlbum = "Hackers"
              }

        eitherDecode' s `shouldBe` Right expected
