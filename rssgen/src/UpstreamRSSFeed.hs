module UpstreamRSSFeed
  ( UpstreamRSSItem(..)
  , parse
  ) where

import Data.List
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Text.XML.Light

-- |A podcast episode of the upstream RSS.
data UpstreamRSSItem = UpstreamRSSItem
  { title :: T.Text
  , pubDate :: UTCTime
  }
  deriving (Eq, Show)

-- |Parses the upstream RSS into items.
parse :: T.Text -> Maybe [UpstreamRSSItem]
parse = parseRSS . parseXML
  where
    parseRSS :: [Content] -> Maybe [UpstreamRSSItem]
    parseRSS contents = do
      let elements = onlyElems contents
      rss <- find ((== unqual "rss") . elName) elements
      channel <- findChild (unqual "channel") rss
      let items = findChildren (unqual "item") channel
      traverse parseRSSItem items

    parseRSSItem :: Element -> Maybe UpstreamRSSItem
    parseRSSItem element = UpstreamRSSItem
      <$> (T.pack . strContent <$> findChild (unqual "title") element)
      <*> (parsePubDate =<< strContent <$> findChild (unqual "pubDate") element)

    parsePubDate :: String -> Maybe UTCTime
    parsePubDate = parseTimeM acceptSurroundingWhitespace defaultTimeLocale rfc822DateFormat
      where acceptSurroundingWhitespace = False
