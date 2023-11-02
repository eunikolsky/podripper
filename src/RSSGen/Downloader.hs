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
  request <- parseRequest url >>= liftIO . applyETag
  response <- httpBS request
  liftIO $ cacheResponse response
  pure $ if responseStatus response == ok200
    then Just $ responseBody response
    else Nothing

  where
    cacheResponse r = let headers = responseHeaders r
      in maybe
        (pure ())
        (setCacheItem conn url)
        (findCacheItem ETag hETag headers <|> findCacheItem LastModified hLastModified headers)

    applyETag r = do
      item <- getCacheItem conn url
      pure $ case item of
        Just (ETag etag) -> r { requestHeaders = requestHeaders r <> [(hIfModifiedSince, etag)] }
        _ -> r

findCacheItem :: (Bytes -> CacheItem) -> HeaderName -> ResponseHeaders -> Maybe CacheItem
findCacheItem mkCacheItem name = fmap (mkCacheItem . snd) . find ((== name) . fst)
