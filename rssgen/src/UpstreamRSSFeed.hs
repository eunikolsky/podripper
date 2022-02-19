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
  , description :: T.Text
  }
  deriving (Eq, Show)

-- |Parses the upstream RSS into items.
parse :: T.Text -> Either String [UpstreamRSSItem]
parse = parseRSS . parseXML
  where
    parseRSS :: [Content] -> Either String [UpstreamRSSItem]
    parseRSS contents = do
      let elements = onlyElems contents
      rss <- find ((== unqual "rss") . elName) elements <?> "no rss"
      channel <- findChild (unqual "channel") rss <?> "no channel"
      items <- ensure (not . null) (findChildren (unqual "item") channel) <?> "no items"
      traverse parseRSSItem items

    parseRSSItem :: Element -> Either String UpstreamRSSItem
    parseRSSItem element = UpstreamRSSItem
      <$> (T.pack . strContent <$> findChild (unqual "title") element) <?> "no title"
      <*> (parsePubDate =<< strContent <$> findChild (unqual "pubDate") element) <?> "no/invalid pubDate"
      <*> (T.pack . strContent <$> findChild (unqual "description") element) <?> "no description"

    parsePubDate :: String -> Maybe UTCTime
    parsePubDate = parseTimeM acceptSurroundingWhitespace defaultTimeLocale rfc822DateFormat
      where acceptSurroundingWhitespace = False

ensure :: (a -> Bool) -> a -> Maybe a
ensure p x = if p x then Just x else Nothing

-- infix 7 <?>
(<?>) :: Maybe a -> b -> Either b a
Just x <?> _ = Right x
Nothing <?> e = Left e
