module RSSGen.PollHTTP
  ( pollHTTP
  ) where

import RSSGen.Downloader

-- | Returns downloaded HTTP file.
--
-- TODO poll for file changes and return it when it's changed
pollHTTP :: (MonadIO m, MonadThrow m, MonadReader Manager m) => URL -> m (Maybe Bytes)
pollHTTP = getFile
