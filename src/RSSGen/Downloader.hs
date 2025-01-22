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
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Function
import Data.List (find)
import Data.Maybe
import Data.Text qualified as T
import Network.HTTP.Client (Request(..), Response(..), parseRequest)
import Network.HTTP.Simple (addRequestHeader)
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
getFile :: (MonadIO m, MonadThrow m, MonadLogger m)
  => (Request -> m (Response Bytes))
  -- ^ the `httpBS` function
  -> DBConnection
  -> URL
  -> m (Maybe Bytes)
getFile httpBS conn url = do
  request <- do
    r <- parseRequest url
    let r' = fixCloudflareETags r
    liftIO $ applyCachedResponse r'
  logD ["Requesting ", show request]
  response <- httpBS request
  logD
    [ "Response status: ", show $ responseStatus response
    , "; headers: ", show $ responseHeaders response
    ]
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

logD :: MonadLogger m => [String] -> m ()
logD = logDebugN . T.pack . mconcat

{-| Works around the unusual cloudflare's behavior with ETags. It would return
 - weak ETags (with the `W/` prefix) to the program and then fail to return
 - `304 Not Modified` with those, even though `curl` would receive strong
 - ETags. The reason turns out to be the `Accept-Encoding: gzip` header,
 - automatically inserted by `http-client`. So one of the possible workarounds
 - for this is to remove that header.
 -}
fixCloudflareETags :: Request -> Request
fixCloudflareETags = addRequestHeader hAcceptEncoding ""
