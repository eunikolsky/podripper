module UpstreamRSSFeedSpec where

import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO

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
        episodes = []
