module RSSItem where

import qualified Data.Text as T
import Data.Time.Clock
import System.Directory
import System.FilePath.Posix

-- | An item in an RSS feed, based on a present file.
data RSSItem = RSSItem
  { file :: FilePath
  , title :: T.Text
  , fileSize :: Integer
  , publishedAt :: UTCTime
  }
  deriving (Show)

-- | Creates an @RSSItem@ based on the information about the file.
rssItemFromFile :: FilePath -> IO RSSItem
rssItemFromFile file = RSSItem
  <$> pure file
  <*> (pure . T.pack . takeFileName) file
  <*> getFileSize file
  <*> getModificationTime file
