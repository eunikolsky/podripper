module RSSGen.DownloaderTypes
  ( Bytes
  , CacheItem(..)
  , URL
  ) where

import qualified Data.ByteString.Lazy as BL
import Database.SQLite.Simple (FromRow(..), field)

type URL = String

type Bytes = BL.ByteString

-- | Cache item, one per URL, stored in the database.
newtype CacheItem = ETag Bytes
  deriving (Eq, Show)

-- TODO it's probably not good that this layer knows about the specific database
-- library
instance FromRow CacheItem where
  fromRow = ETag <$> field
