module RSSGen.PollHTTPSpec where

import Data.Functor.Identity
import RSSGen.Downloader
import RSSGen.PollHTTP
import Test.Hspec

spec :: Spec
spec = describe "pollHTTP" $
  it "returns downloaded data" $
    (runIdentity . runFakeDownloadT $ pollHTTP "https://example.org/")
      `shouldBe` Just "<!-- https://example.org/ -->"
