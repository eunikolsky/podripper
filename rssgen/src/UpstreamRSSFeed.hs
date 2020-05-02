module UpstreamRSSFeed
  ( UpstreamRSSItem
  , parse
  ) where

import qualified Data.Text as T
import Data.Time.Clock
import Text.XML.Light

-- |A podcast episode of the upstream RSS.
data UpstreamRSSItem = UpstreamRSSItem
  { title :: T.Text
  , pubDate :: UTCTime
  }
  deriving (Eq, Show)

-- |Parses the upstream RSS into items.
parse :: T.Text -> Maybe [UpstreamRSSItem]
parse = const $ pure []
