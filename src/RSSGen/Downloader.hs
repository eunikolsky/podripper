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
import Data.Function
import Data.List (find)
import Data.Maybe
import Network.HTTP.Client (Request(..), Response(..), parseRequest)
import Network.HTTP.Types
import Network.HTTP.Types.Header
import RSSGen.Database
import RSSGen.DownloaderTypes

-- |Downloads a file by the `URL` with an automatic response caching support,
-- that is, the `ETag` and `Last-Modified` response headers (with the
-- corresponding `If-None-Match` and `If-Modified-Since` request headers) if
-- present, or the body itself otherwise. Returns the response's body on success
-- and only if the body has changed since the last response, and `Nothing`
-- otherwise.
getFile :: (MonadIO m, MonadThrow m)
  => (Request -> m (Response Bytes))
  -- ^ the `httpBS` function
  -> DBConnection
  -> URL
  -> m (Maybe Bytes)
getFile httpBS conn url = do
  request <- parseRequest url >>= liftIO . applyCachedResponse
  response <- httpBS request
  let responseSuccessful = responseStatus response == ok200

  maybeCachedBody <- liftIO getCachedBody
  when responseSuccessful $ liftIO $ cacheResponse response

  let body = responseBody response
      -- note: `hlint` was smart to suggest replacing `maybe True (body /=)`
      -- with `(Just body /=)`
      bodyHasChanged = Just body /= maybeCachedBody
  pure $ if responseSuccessful && bodyHasChanged
    then Just body
    else Nothing

  where
    cacheResponse r =
      let headers = responseHeaders r
          maybeETag = findHeaderValue hETag headers
          maybeLastModified = findHeaderValue hLastModified headers
          cacheItem = asum
            [ ETagWithLastModified <$> maybeETag <*> maybeLastModified
            , ETag <$> maybeETag
            , LastModified <$> maybeLastModified
            ]
            & fromMaybe (Body $ responseBody r)
      in setCacheItem conn url cacheItem

    applyCachedResponse r = do
      item <- getCacheItem conn url
      pure $ case item of
        Just (ETag etag) -> r { requestHeaders =
          requestHeaders r <> [(hIfNoneMatch, etag)] }
        Just (LastModified lastmod) -> r { requestHeaders =
          requestHeaders r <> [(hIfModifiedSince, lastmod)] }
        Just (ETagWithLastModified etag lastmod) -> r { requestHeaders =
          requestHeaders r <> [(hIfNoneMatch, etag), (hIfModifiedSince, lastmod)] }
        _ -> r

    getCachedBody = do
      item <- getCacheItem conn url
      pure $ case item of
        Just (Body body) -> Just body
        _ -> Nothing

findHeaderValue :: HeaderName -> ResponseHeaders -> Maybe Bytes
findHeaderValue name = fmap snd . find ((== name) . fst)
