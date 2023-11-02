module RSSGen.Downloader
  ( getFile

  -- * re-exports
  , Bytes
  , MonadIO
  , MonadReader
  , MonadThrow
  , URL
  ) where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.List (find)
import Network.HTTP.Client (Request(..), Response(..), parseRequest)
import Network.HTTP.Types
import Network.HTTP.Types.Header
import RSSGen.Database
import RSSGen.DownloaderTypes

-- |Downloads a file by the `URL`, returns the response's body on success, or
-- `Nothing` otherwise.
getFile :: (MonadIO m, MonadThrow m)
  => (Request -> m (Response Bytes))
  -- ^ the `httpBS` function
  -> Connection
  -- ^ database connection
  -> URL
  -> m (Maybe Bytes)
getFile httpBS conn url = do
  request <- parseRequest url >>= liftIO . applyCachedResponse
  response <- httpBS request
  liftIO $ cacheResponse response
  pure $ if responseStatus response == ok200
    then Just $ responseBody response
    else Nothing

  where
    cacheResponse r =
      let headers = responseHeaders r
          maybeETag = findHeaderValue hETag headers
          maybeLastModified = findHeaderValue hLastModified headers
          maybeCacheItem = asum
            [ ETagWithLastModified <$> maybeETag <*> maybeLastModified
            , ETag <$> maybeETag
            , LastModified <$> maybeLastModified
            ]
      in maybe
        (pure ())
        (setCacheItem conn url)
        maybeCacheItem

    applyCachedResponse r = do
      item <- getCacheItem conn url
      pure $ case item of
        Just (ETag etag) -> r { requestHeaders = requestHeaders r <> [(hIfModifiedSince, etag)] }
        Just (LastModified lastmod) -> r { requestHeaders = requestHeaders r <> [(hIfNoneMatch, lastmod)] }
        _ -> r

findHeaderValue :: HeaderName -> ResponseHeaders -> Maybe Bytes
findHeaderValue name = fmap snd . find ((== name) . fst)
