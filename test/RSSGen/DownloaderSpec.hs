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
    let verifyStored item = do
          let url = "http://localhost"
              mockHTTPBS _ = pure $ responseWith item

          actual <- withDB $ \conn -> do
            void $ getFile mockHTTPBS conn url
            liftIO $ getCacheItem conn url

          actual `shouldBe` Just item

    it "stores ETag from a response" $ verifyStored $ ETag "etag"

    it "stores Last-Modified from a response" $ verifyStored $ LastModified "last-modified"

    it "stores ETag and Last-Modified from a response" $ verifyStored $
      ETagWithLastModified "etag" "last-modified"

    it "sets If-Modified-Since with stored ETag" $ do
      ifModifiedSinceRef <- newIORef Nothing

      let url = "http://localhost"
          etag = ETag "etag"
          mockHTTPBS req = do
            writeIORef ifModifiedSinceRef .
              findHeaderValue "If-Modified-Since" $ requestHeaders req
            pure $ responseWith etag

      withDB $ \conn -> do
        liftIO $ setCacheItem conn url etag
        void $ getFile mockHTTPBS conn url

      actual <- readIORef ifModifiedSinceRef
      ETag <$> actual `shouldBe` Just etag

    it "sets If-None-Match with stored LastModified" $ do
      ifNoneMatchRef <- newIORef Nothing

      let url = "http://localhost"
          lastmod = LastModified "last-modified"
          mockHTTPBS req = do
            writeIORef ifNoneMatchRef .
              findHeaderValue "If-None-Match" $ requestHeaders req
            pure $ responseWith lastmod

      withDB $ \conn -> do
        liftIO $ setCacheItem conn url lastmod
        void $ getFile mockHTTPBS conn url

      actual <- readIORef ifNoneMatchRef
      LastModified <$> actual `shouldBe` Just lastmod

responseWith :: CacheItem -> Response Bytes
responseWith item = Response
  { responseHeaders
  , responseStatus = ok200
  , responseVersion = http11
  , responseBody = ""
  , responseCookieJar = mempty
  , responseClose' = undefined
  , responseOriginalRequest = undefined
  }

  where
    responseHeaders = case item of
      ETag etag -> [("ETag", etag)]
      LastModified lastmod -> [("Last-Modified", lastmod)]
      ETagWithLastModified etag lastmod -> [("ETag", etag), ("Last-Modified", lastmod)]
      _ -> mempty

findHeaderValue :: HeaderName -> RequestHeaders -> Maybe Bytes
findHeaderValue name = fmap snd . find ((== name) . fst)

withDB :: MonadIO m => (Connection -> m a) -> m a
withDB io = do
  conn <- liftIO $ openDatabase InMemory
  actual <- io conn
  liftIO $ closeDatabase conn
  pure actual
