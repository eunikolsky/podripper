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
    context "given 200 OK response" $ do
      let responseWith' = responseWith ok200
          verifyStored item = do
            let url = "http://localhost"
                mockHTTPBS _ = pure $ responseWith' item

            actual <- withDB $ \conn -> do
              void $ getFile mockHTTPBS conn url
              liftIO $ getCacheItem conn url

            actual `shouldBe` Just item

      it "stores ETag from a response" $ verifyStored $ ETag "etag"

      it "stores Last-Modified from a response" $ verifyStored $ LastModified "last-modified"

      it "stores ETag and Last-Modified from a response" $ verifyStored $
        ETagWithLastModified "etag" "last-modified"

      it "stores Body of a response" $ verifyStored $ Body "response body"

      it "sets If-Modified-Since with stored ETag" $ do
        ifModifiedSinceRef <- newIORef Nothing

        let url = "http://localhost"
            etag = ETag "etag"
            mockHTTPBS req = do
              writeIORef ifModifiedSinceRef .
                findHeaderValue "If-Modified-Since" $ requestHeaders req
              pure $ responseWith' etag

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
              pure $ responseWith' lastmod

        withDB $ \conn -> do
          liftIO $ setCacheItem conn url lastmod
          void $ getFile mockHTTPBS conn url

        actual <- readIORef ifNoneMatchRef
        LastModified <$> actual `shouldBe` Just lastmod

      it "sets Is-Modified-Since and If-None-Match with stored ETagWithLastModified" $ do
        headerValuesRef <- newIORef Nothing

        let url = "http://localhost"
            etagLastmod = ETagWithLastModified "etag" "last-modified"
            mockHTTPBS req = do
              writeIORef headerValuesRef $ sequence
                ( findHeaderValue "If-Modified-Since" $ requestHeaders req
                , findHeaderValue "If-None-Match" $ requestHeaders req
                )
              pure $ responseWith' etagLastmod

        withDB $ \conn -> do
          liftIO $ setCacheItem conn url etagLastmod
          void $ getFile mockHTTPBS conn url

        actual <- readIORef headerValuesRef
        let (ifModifiedSince, ifNoneMatch) = sequence actual
        ETagWithLastModified <$> ifModifiedSince <*> ifNoneMatch `shouldBe` Just etagLastmod

      it "returns body when response Body has changed from cached" $ do
        let url = "http://localhost"
            cachedBody = Body "previous body"
            body = "response body"
            mockHTTPBS _ = pure $ responseWith' (Body body)

        actual <- withDB $ \conn -> do
          liftIO $ setCacheItem conn url cachedBody
          getFile mockHTTPBS conn url

        actual `shouldBe` Just body

      it "returns body when response Body wasn't cached" $ do
        let url = "http://localhost"
            body = "response body"
            mockHTTPBS _ = pure $ responseWith' (Body body)

        actual <- withDB $ \conn -> getFile mockHTTPBS conn url

        actual `shouldBe` Just body

      it "returns Nothing when response Body hasn't changed from cached" $ do
        let url = "http://localhost"
            body = Body "response body"
            mockHTTPBS _ = pure $ responseWith' body

        actual <- withDB $ \conn -> do
          liftIO $ setCacheItem conn url body
          getFile mockHTTPBS conn url

        actual `shouldBe` Nothing

    context "given 304 Not modified response" $ do
      it "returns Nothing" $ do
        let url = "http://localhost"
            mockHTTPBS _ = pure $ response notModified304

        actual <- withDB $ \conn -> getFile mockHTTPBS conn url

        actual `shouldBe` Nothing

    context "given an error response" $ do
      let verifyNotStored status item = do
            let url = "http://localhost"
                mockHTTPBS _ = pure $ responseWith status item

            actual <- withDB $ \conn -> do
              void $ getFile mockHTTPBS conn url
              liftIO $ getCacheItem conn url

            actual `shouldBe` Nothing

      it "returns Nothing" $ do
        let url = "http://localhost"
            mockHTTPBS _ = pure $ response imATeapot418

        actual <- withDB $ \conn -> getFile mockHTTPBS conn url

        actual `shouldBe` Nothing

      it "doesn't store ETag from a response" $
        verifyNotStored internalServerError500 $ ETag "etag"

      it "doesn't store Last-Modified from a response" $
        verifyNotStored badRequest400 $ LastModified "last-modified"

      it "doesn't store ETag and Last-Modified from a response" $
        verifyNotStored badGateway502 $ ETagWithLastModified "etag" "last-modified"

      it "doesn't store Body of a response" $
        verifyNotStored notFound404 $ Body "body"

responseWith :: Status -> CacheItem -> Response Bytes
responseWith responseStatus item = Response
  { responseHeaders
  , responseStatus
  , responseVersion = http11
  , responseBody
  , responseCookieJar = mempty
  , responseClose' = undefined
  , responseOriginalRequest = undefined
  }

  where
    responseHeaders = case item of
      ETag etag -> [("ETag", etag)]
      LastModified lastmod -> [("Last-Modified", lastmod)]
      ETagWithLastModified etag lastmod -> [("ETag", etag), ("Last-Modified", lastmod)]
      Body _ -> mempty

    responseBody = case item of
      Body body -> body
      _ -> mempty

response :: Status -> Response Bytes
response responseStatus = Response
  { responseHeaders = mempty
  , responseStatus
  , responseVersion = http11
  , responseBody = mempty
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
