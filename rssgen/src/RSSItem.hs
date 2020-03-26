module RSSItem where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
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

-- | Creates an @RSSItem@ based on the information about the file. Returns
-- @Nothing@ if the file is not found.
rssItemFromFile :: FilePath -> IO (Maybe RSSItem)
rssItemFromFile file = runMaybeT $ do
  existingFile <- MaybeT $ doesFileExist' file
  fileSize <- liftIO $ getFileSize existingFile
  modTime <- liftIO $ getModificationTime existingFile

  return $ RSSItem
    { file = existingFile
    , title = T.pack . takeFileName $ existingFile
    , fileSize = fileSize
    , publishedAt = modTime
    }

-- | Returns the filename if it exists.
doesFileExist' :: FilePath -> IO (Maybe FilePath)
doesFileExist' file = do
  exists <- doesFileExist file
  return $ if exists
    then Just file
    else Nothing
