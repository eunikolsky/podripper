module RSSGen.DownloaderTypes
  ( Bytes
  , CacheItem(..)
  , URL
  ) where

import qualified Data.ByteString.Lazy as BL

type URL = String

type Bytes = BL.ByteString

-- | Cache item, one per URL, stored in the database.
data CacheItem
  = ETag Bytes
  | LastModified Bytes
  | ETagWithLastModified Bytes Bytes
  deriving (Eq, Show)
