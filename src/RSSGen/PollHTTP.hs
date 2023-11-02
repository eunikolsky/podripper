module RSSGen.PollHTTP
  ( pollHTTP
  ) where

import Network.HTTP.Client
import RSSGen.Database
import RSSGen.Downloader

-- | Returns downloaded HTTP file.
--
-- TODO poll for file changes and return it when it's changed
pollHTTP :: (MonadIO m, MonadThrow m)
  => (Request -> m (Response Bytes))
  -- ^ the `httpBS` function
  -> Connection
  -- ^ database connection
  -> URL
  -> m (Maybe Bytes)
pollHTTP = getFile
