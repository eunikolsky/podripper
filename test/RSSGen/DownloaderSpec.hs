module RSSGen.DownloaderSpec where

import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Client.Internal (Response(..))
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import RSSGen.Database
import RSSGen.Downloader
import RSSGen.DownloaderTypes
import Test.Hspec

spec :: Spec
spec = do
  describe "getFile" $ do
    it "stores ETag from a response" $ do
      let url = "http://localhost"
          etag = "etag"
          mockHTTPBS _ = pure $ responseWithETag etag

      actual <- withDB $ \conn -> do
        void $ getFile mockHTTPBS conn url
        liftIO $ getCacheItem conn url

      actual `shouldBe` Just (ETag etag)

responseWithETag :: Bytes -> Response Bytes
responseWithETag etag = Response
  { responseHeaders = [("ETag", etag)]
  , responseStatus = ok200
  , responseVersion = http11
  , responseBody = ""
  , responseCookieJar = mempty
  , responseClose' = undefined
  , responseOriginalRequest = undefined
  }

withDB :: MonadIO m => (Connection -> m a) -> m a
withDB io = do
  conn <- liftIO $ openDatabase InMemory
  actual <- io conn
  liftIO $ closeDatabase conn
  pure actual
