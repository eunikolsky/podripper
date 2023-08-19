{-# LANGUAGE QuasiQuotes #-}

module RipConfigSpec where

import Data.Aeson
import RipConfig
import Test.Hspec
import Text.RawString.QQ
import Data.Text.Lazy.Encoding (encodeUtf8)

spec :: Spec
spec = do
  describe "RipConfig" $ do
    it "can be parsed from json" $ do
      let s = encodeUtf8 [r|{
        "streamURL": "http://example.org",
        "durationSec": 4,
        "retrySec": 8,
        "ripDirName": "test",
        "podArtist": "Хакер",
        "podAlbum": "Hackers"
      }|]

      let expected = RipConfig
            { streamURL = "http://example.org"
            , durationSec = 4
            , retrySec = 8
            , ripDirName = "test"
            , podArtist = "Хакер"
            , podAlbum = "Hackers"
            }

      eitherDecode' s `shouldBe` Right expected
