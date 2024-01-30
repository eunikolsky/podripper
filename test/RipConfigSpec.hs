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
      it "parses json with stream url" $ do
        let s = encodeUtf8 [r|{
          "stream": { "url": "http://example.org" },
          "ripIntervals": [
            "Mo 13:00-15:00 America/Toronto: 2m"
          ],
          "defaultRipperDelay": "2m",
          "postRipEndDelays": [
            "[< 5m]: 1s",
            "[< 15m]: 3s"
          ],
          "noDataTimeout": "8s",
          "ripDirName": "test",
          "podArtist": "Хакер",
          "podAlbum": "Hackers",
          "podLanguage": "eng",
          "podHomepageLatin1": "https://example.org",
          "oldFileDays": 10
        }|]

        let expected = RipConfig
              { streamURLConfig = StreamWithURL . StreamURL $ URL "http://example.org"
              , ripIntervalRefs =
                [ RipperIntervalRef Monday (read "13:00:00", read "15:00:00") "America/Toronto" (RetryDelay $ durationMinutes 2)
                ]
              , defaultRipperDelay = RetryDelay $ durationMinutes 2
              , postRipEndDelays =
                [ PostRipEndDelay (durationMinutes 5) (RetryDelay $ durationSeconds 1)
                , PostRipEndDelay (durationMinutes 15) (RetryDelay $ durationSeconds 3)
                ]
              , noDataTimeout = durationSeconds 8
              , ripDirName = "test"
              , podArtist = "Хакер"
              , podAlbum = "Hackers"
              , podLanguage = "eng"
              , podHomepage = "https://example.org"
              , oldFileDays = 10
              }

        eitherDecode' s `shouldBe` Right expected

      it "parses json with stream with live check" $ do
        let s = encodeUtf8 [r|{
          "stream": {
            "checkURL": "http://example.org/check",
            "liveKey": "live",
            "playerKey": "player"
          },
          "ripIntervals": [
            "Mo 13:00-15:00 America/Toronto: 2m"
          ],
          "defaultRipperDelay": "2m",
          "postRipEndDelays": [
            "[< 5m]: 1s",
            "[< 15m]: 3s"
          ],
          "noDataTimeout": "8s",
          "ripDirName": "test",
          "podArtist": "Хакер",
          "podAlbum": "Hackers",
          "podLanguage": "eng",
          "podHomepageLatin1": "https://example.org",
          "oldFileDays": 10
        }|]

        let expected = RipConfig
              { streamURLConfig = StreamWithLiveCheck $ StreamCheckConfig
                { checkURL = StreamCheckURL $ URL "http://example.org/check"
                , liveKey = "live"
                , playerKey = "player"
                }
              , ripIntervalRefs =
                [ RipperIntervalRef Monday (read "13:00:00", read "15:00:00") "America/Toronto" (RetryDelay $ durationMinutes 2)
                ]
              , defaultRipperDelay = RetryDelay $ durationMinutes 2
              , postRipEndDelays =
                [ PostRipEndDelay (durationMinutes 5) (RetryDelay $ durationSeconds 1)
                , PostRipEndDelay (durationMinutes 15) (RetryDelay $ durationSeconds 3)
                ]
              , noDataTimeout = durationSeconds 8
              , ripDirName = "test"
              , podArtist = "Хакер"
              , podAlbum = "Hackers"
              , podLanguage = "eng"
              , podHomepage = "https://example.org"
              , oldFileDays = 10
              }

        eitherDecode' s `shouldBe` Right expected

      it "ignores unknown fields" $ do
        let s = encodeUtf8 [r|{
          "stream": { "url": "http://example.org" },
          "_comment_duration": "just 4",
          "ripDirName": "test",
          "ripIntervals": [],
          "defaultRipperDelay": "1h",
          "postRipEndDelays": [],
          "noDataTimeout": "1h",
          "podArtist": "Хакер",
          "podAlbum": "Hackers",
          "podLanguage": "eng",
          "podHomepageLatin1": "https://example.org",
          "oldFileDays": 1,
          "?": "?"
        }|]

        let expected = RipConfig
              { streamURLConfig = StreamWithURL . StreamURL $ URL "http://example.org"
              , ripIntervalRefs = []
              , defaultRipperDelay = RetryDelay $ durationHours 1
              , postRipEndDelays = []
              , noDataTimeout = durationHours 1
              , ripDirName = "test"
              , podArtist = "Хакер"
              , podAlbum = "Hackers"
              , podLanguage = "eng"
              , podHomepage = "https://example.org"
              , oldFileDays = 1
              }

        eitherDecode' s `shouldBe` Right expected
