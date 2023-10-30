module RSSGen.PollHTTPSpec where

import Data.ByteString.Lazy.Char8 qualified as CL
import Data.Functor.Identity
import RSSGen.Downloader
import RSSGen.PollHTTP
import Test.Hspec

spec :: Spec
spec = describe "pollHTTP" $
  it "returns downloaded data" $
    (runIdentity . runFakeDownloadT $ pollHTTP "https://example.org/")
      `shouldBe` Just "<!-- https://example.org/ -->"

-- |A fake downloader that always returns an XML with the commented URL.
newtype FakeDownloadT m a = FakeDownloadT { runFakeDownloadT :: m a }
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadDownload (FakeDownloadT m) where
  getFile url = pure . Just $ "<!-- " <> CL.pack url <> " -->"
