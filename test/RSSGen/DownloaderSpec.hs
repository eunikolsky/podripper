module RSSGen.DownloaderSpec where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.List (find)
import Network.HTTP.Client
import Network.HTTP.Client.Internal (Response(..))
import Network.HTTP.Types
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

    it "sets If-Modified-Since with stored ETag" $ do
      ifModifiedSinceRef <- newIORef Nothing

      let url = "http://localhost"
          etag = "etag"
          mockHTTPBS req = do
            writeIORef ifModifiedSinceRef .
              findHeaderValue "If-Modified-Since" $ requestHeaders req
            pure $ responseWithETag etag

      withDB $ \conn -> do
        liftIO $ setCacheItem conn url (ETag etag)
        void $ getFile mockHTTPBS conn url

      actual <- readIORef ifModifiedSinceRef
      actual `shouldBe` Just etag

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

findHeaderValue :: HeaderName -> RequestHeaders -> Maybe Bytes
findHeaderValue name = fmap snd . find ((== name) . fst)

withDB :: MonadIO m => (Connection -> m a) -> m a
withDB io = do
  conn <- liftIO $ openDatabase InMemory
  actual <- io conn
  liftIO $ closeDatabase conn
  pure actual
