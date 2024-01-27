{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RSSGen.RSSItem
  ( RSSItem(..)
  , RipFile(..)
  , RipTime
  , UTCRipTime
  , renderItem
  , ripFileFromFile
  , rssItemFromRipFile
  , toUTCTime
  , utcRipTime
  , zonedTime
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Function
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time
import Development.Shake.Classes
import Rip
import System.Directory
import System.FilePath.Posix
import Text.XML.Light

import qualified RSSGen.UpstreamRSSFeed as UpstreamRSSFeed

newtype UTCRipTime = UTCRipTime { toUTCTime :: UTCTime }
  deriving newtype (Show, Eq, Ord, Hashable, Binary, NFData)

-- | Time of a rip file, extracted from its filename.
data RipTime = RipTime
  { zonedTime :: !ZonedTime
  , utcRipTime :: !UTCRipTime
  }
  deriving (Show)

instance Eq RipTime where
  (==) = (==) `on` utcRipTime

mkRipTime :: (ZonedTime, UTCTime) -> RipTime
mkRipTime (zonedTime, utcTime) = RipTime{zonedTime, utcRipTime=UTCRipTime utcTime}

-- | Information about a rip file, which is read from the filesystem.
data RipFile = RipFile
  { file :: !FilePath
  , fileSize :: !Integer
  , ripTime :: !RipTime
  }
  deriving (Show, Eq)

instance Ord RipFile where
  -- is this a valid definition given that `Eq` compares all the fields?
  (<=) = (<=) `on` utcRipTime . ripTime

-- | An item in an RSS feed, based on a present file, including information from
-- an upstream RSS item if any.
data RSSItem = RSSItem
  { ripFile :: !RipFile
  , title :: !T.Text
  , description :: !(Maybe T.Text)
  }
  deriving (Show, Eq)

-- | Formats the publication date string as required in the RSS standard.
formatPubDate :: ZonedTime -> String
formatPubDate = formatTime defaultTimeLocale "%d %b %Y %H:%M:%S %z"

-- | Formats the publication date for the RSS item's title, specifically in
-- the `YYYY-MM-DD` format so that the files appear sorted on the podcast
-- player when synced with gPodder (which renames the files on the device
-- based on the title).
titlePubDate :: ZonedTime -> String
titlePubDate = formatTime defaultTimeLocale "%F %T %z"

-- | Creates an @RipFile@ based on the information about the file located in
-- `relPath`. Returns @Nothing@ if the file is not found.
ripFileFromFile :: FilePath -> FilePath -> IO (Maybe RipFile)
ripFileFromFile relPath filename = runMaybeT $ do
  filename' <- MaybeT . doesFileExist' $ relPath </> filename
  fileSize <- liftIO $ getFileSize filename'
  localTime <- MaybeT . pure . parseRipDate $ filename'
  ripTime <- liftIO . fmap mkRipTime $ localTimeToZonedTime localTime
  pure RipFile{file=filename,..}

-- | Creates an @RSSItem@ from the `RipFile` by adding extra information.
-- @findUpstreamItem@ is used to find an upstream RSS item that is close
-- to the item's date; if there is one, uses its title, otherwise uses the
-- @podcastTitle@.
rssItemFromRipFile :: UpstreamRSSFeed.PodcastId -> (UTCTime -> IO (Maybe UpstreamRSSFeed.UpstreamRSSItem)) -> RipFile -> IO RSSItem
rssItemFromRipFile podcastTitle findUpstreamItem ripFile@RipFile{..} = do
  maybeUpstreamItem <- findUpstreamItem . toUTCTime . utcRipTime $ ripTime
  let title = T.pack $ mconcat
        [ titlePubDate $ zonedTime ripTime
        , " / "
        , T.unpack $ (UpstreamRSSFeed.title <$> maybeUpstreamItem) ?? podcastTitle
        ]
      description = UpstreamRSSFeed.description <$> maybeUpstreamItem

  return $ RSSItem {..}

-- | `Maybe`-coalescing operator.
(??) :: Maybe a -> a -> a
(??) = flip fromMaybe

-- | Renders the RSS item into an XML element for the feed. First parameter
-- is the base URL for the file.
renderItem :: String -> RSSItem -> Element
renderItem baseURL RSSItem{ripFile=RipFile{..},..} = unode "item" [ititle, guid, idescription, pubDate, enclosure]
  where
    ititle = unode "title" $ T.unpack title
    guid = unode "guid" (Attr (unqual "isPermaLink") "false", takeFileName file)
    idescription = maybe (unode "description" ()) (unode "description" . flip (CData CDataVerbatim) Nothing . T.unpack) description
    pubDate = unode "pubDate" . formatPubDate . zonedTime $ ripTime
    enclosure = unode "enclosure"
      [ Attr (unqual "url") $ baseURL <> file
      , Attr (unqual "type") "audio/mp3"
      , Attr (unqual "length") $ show fileSize
      ]

-- | Returns the filename if it exists.
doesFileExist' :: FilePath -> IO (Maybe FilePath)
doesFileExist' file = do
  exists <- doesFileExist file
  return $ if exists
    then Just file
    else Nothing
