module RSSGen.DownloaderTypes
  ( Bytes
  , URL
  ) where

import qualified Data.ByteString.Lazy as BL

type URL = String

type Bytes = BL.ByteString
