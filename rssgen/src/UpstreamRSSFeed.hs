{-# LANGUAGE TypeApplications #-}

module UpstreamRSSFeed
  ( PodcastId
  , UpstreamRSSItem(..)
  , parse
  ) where

import Data.List
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Database.SQLite.Simple
import Text.XML.Light

-- |The podcast name to which the RSS item belongs.
type PodcastId = T.Text

-- |A podcast episode of the upstream RSS.
data UpstreamRSSItem = UpstreamRSSItem
  { title :: T.Text
  , pubDate :: UTCTime
  , description :: T.Text
  , guid :: T.Text
  , podcast :: PodcastId
  }
  deriving (Eq, Show)

instance FromRow UpstreamRSSItem where
  fromRow = do
    podcast <- field
    title <- field
    description <- field
    guid <- field
    publishedAt <- field @Int
    let pubDate = parseTimeOrError acceptSurroundingWhitespace defaultTimeLocale "%s" (show publishedAt)
    pure $ UpstreamRSSItem title pubDate description guid podcast

instance ToRow UpstreamRSSItem where
  toRow (UpstreamRSSItem title pubDate description guid podcast) = toRow
    ( podcast
    , title
    , description
    , guid
    , formatTime defaultTimeLocale "%s" pubDate
    )

-- |Parses the upstream RSS into items.
parse :: PodcastId -> T.Text -> Either String [UpstreamRSSItem]
parse podcast = parseRSS . parseXML
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
      <*> (T.pack . strContent <$> findChild (unqual "guid") element) <?> "no guid"
      <*> pure podcast

    parsePubDate :: String -> Maybe UTCTime
    parsePubDate = parseTimeM acceptSurroundingWhitespace defaultTimeLocale rfc822DateFormat

acceptSurroundingWhitespace = False

ensure :: (a -> Bool) -> a -> Maybe a
ensure p x = if p x then Just x else Nothing

-- infix 7 <?>
(<?>) :: Maybe a -> b -> Either b a
Just x <?> _ = Right x
Nothing <?> e = Left e
