module RSSGen.DownloaderTypes
  ( Bytes
  , CacheItem(..)
  , URL
  ) where

import qualified Data.ByteString as B

-- FIXME use the other `URL` type
type URL = String

type Bytes = B.ByteString

-- | Cache item, one per URL, stored in the database.
data CacheItem
  -- these three options are server-supported cache items because has to support
  -- `ETag` and/or `Last-Modified`, so we can store some small data
  = ETag Bytes
  | LastModified Bytes
  | ETagWithLastModified Bytes Bytes
  -- this option is for servers that don't support those headers, so we have to
  -- store the entire body in order not to re-process it again if it hasn't
  -- changed since the previous response
  | Body Bytes
  deriving (Eq, Show)
