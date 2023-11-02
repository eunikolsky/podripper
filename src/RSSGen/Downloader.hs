module RSSGen.Downloader
  ( getFile

  -- * re-exports
  , Bytes
  , MonadIO
  , MonadReader
  , MonadThrow
  , URL
  ) where

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
  liftIO $ storeETag response
  pure $ if responseStatus response == ok200
    then Just $ responseBody response
    else Nothing

  where
    storeETag r = maybe
      (pure ())
      (setCacheItem conn url . ETag)
      (findHeaderValue hETag $ responseHeaders r)

    applyETag r = do
      item <- getCacheItem conn url
      pure $ case item of
        Just (ETag etag) -> r { requestHeaders = requestHeaders r <> [(hIfModifiedSince, etag)] }
        _ -> r

findHeaderValue :: HeaderName -> ResponseHeaders -> Maybe Bytes
findHeaderValue name = fmap snd . find ((== name) . fst)
