{-# LANGUAGE QuasiQuotes #-}

module RipConfigSpec where

import Data.Aeson
import Data.Time
import RSSGen.Duration
import RipConfig
import Ripper.RipperDelay
import Ripper.Types
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
          "ripIntervals": [
            "Mo 13:00-15:00 America/Toronto: 2m"
          ],
          "ripDirName": "test",
          "podArtist": "Хакер",
          "podAlbum": "Hackers"
        }|]

        let expected = RipConfig
              { streamURL = StreamURL $ URL "http://example.org"
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
          "_comment_duration": "just 4",
          "ripDirName": "test",
          "ripIntervals": [],
          "podArtist": "Хакер",
          "podAlbum": "Hackers",
          "?": "?"
        }|]

        let expected = RipConfig
              { streamURL = StreamURL $ URL "http://example.org"
              , ripIntervalRefs = []
              , ripDirName = "test"
              , podArtist = "Хакер"
              , podAlbum = "Hackers"
              }

        eitherDecode' s `shouldBe` Right expected
