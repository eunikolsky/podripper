{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Data.Maybe
import Data.Ord (Down(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Time.Clock
import Data.Time.Format
import Data.Version (Version, showVersion)
import Database.SQLite.Simple
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified System.Environment as Env

import qualified Paths_rssgen as Paths (version)
import Downloader
import RSSFeed
import RSSItem
import qualified UpstreamRSSFeed

newtype RSSGenVersion = RSSGenVersion ()
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult RSSGenVersion = Version

-- |This oracle is to depend on the upstream RSS without an intermediate file.
-- If the downloading failed, we don't want to fail our build.
-- URL -> Maybe T.Text
newtype UpstreamRSS = UpstreamRSS URL
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult UpstreamRSS = Maybe T.Text

main :: IO ()
main = do
  shakeDir <- fromMaybe "/var/lib/podripper/shake" <$> Env.lookupEnv "SHAKE_DIR"
  shakeArgs shakeOptions { shakeFiles = shakeDir, shakeColor = True } $ do
    want $ ["radiot", "rcmp"] <&> (<.> "rss")

    getRSSGenVersion <- addOracle $ \RSSGenVersion{} -> return Paths.version

    upstreamRSS <- addOracle $ \(UpstreamRSS url) -> do
      manager <- liftIO $ newManager tlsManagerSettings
      -- this `liftIO` is to fix the build error:
      -- • No instance for (exceptions-0.10.4:Control.Monad.Catch.MonadThrow
      --                      Action)
      --     arising from a use of ‘downloadRadioTRSS’
      liftIO $ runReaderT (runHTTPClientDownloadT $ downloadRSS url) manager

    versioned 16 $ "*.rss" %> \out -> do
      getRSSGenVersion $ RSSGenVersion ()

      configDir <- getEnvWithDefault "/usr/share/podripper" "CONF_DIR"
      let podcastTitle = dropExtension out
          feedConfigFile = configDir </> podcastTitle <> "_feed.json"
      need [feedConfigFile]
      feedConfig <- fmap decode . liftIO . BL.readFile $ feedConfigFile
      case feedConfig of
        Just config -> do
          conn <- liftIO openDatabase

          -- downloading is triggered every time when this RSS is requested, and
          -- the RSS is not updated if the upstream RSS hasn't changed
          maybeUpstreamRSS <- runMaybeT $ do
            url <- MaybeT . pure $ upstreamRSSURL config :: MaybeT Action T.Text
            text <- MaybeT . upstreamRSS . UpstreamRSS . T.unpack $ url
            items <- MaybeT . pure . eitherToMaybe . UpstreamRSSFeed.parse (T.pack podcastTitle) $ text
            -- if we're here, then we have items
            liftIO $ saveUpstreamRSSItems conn items
            pure items

          generateFeed config conn out
          liftIO $ close conn
        Nothing -> fail $ "Couldn't parse feed config file " <> feedConfigFile

    where
      generateFeed :: RSSFeedConfig -> Connection -> FilePattern -> Action ()
      generateFeed feedConfig conn out = do
        -- we need the audio files to generate the RSS, which are in the
        -- directory of the same name as the podcast title
        let podcastTitle = dropExtension out
        mp3Files <- getDirectoryFiles "" [podcastTitle </> "*.mp3"]
        let findUpstreamItem = closestUpstreamItemToTime (T.pack podcastTitle) conn
        rssItems <- liftIO . fmap (newestFirst . catMaybes) $ traverse (rssItemFromFile podcastTitle findUpstreamItem) mp3Files

        version <- askOracle $ RSSGenVersion ()
        writeFile' out $ feed (ProgramVersion . showVersion $ version) feedConfig rssItems

      newestFirst :: [RSSItem] -> [RSSItem]
      newestFirst = sortOn Down

-- |Opens the episodes database and ensures the `episode` table is created.
openDatabase :: IO Connection
openDatabase = do
  conn <- open "episodes.sqlite"
  execute_ conn
    "CREATE TABLE IF NOT EXISTS episode (\
    \ id INTEGER PRIMARY KEY ASC NOT NULL,\
    \ podcast TEXT NOT NULL,\
    \ title TEXT NOT NULL,\
    \ description TEXT NOT NULL,\
    \ guid TEXT NOT NULL,\
    \ publishedAt INTEGER NOT NULL,\
    \ addedAt INTEGER NOT NULL DEFAULT (strftime('%s', 'now')),\
    \ UNIQUE (podcast, title, description, guid, publishedAt) ON CONFLICT IGNORE)"
  pure conn

-- |Saves all the parsed upstream RSS items into the database. The `episode`
-- |table ignores duplicate records.
saveUpstreamRSSItems :: Connection -> [UpstreamRSSFeed.UpstreamRSSItem] -> IO ()
saveUpstreamRSSItems conn
  = executeMany conn "INSERT INTO episode (podcast,title,description,guid,publishedAt) VALUES (?,?,?,?,?)"
  -- this provides a small benefit of the initially imported items being
  -- sorted from lower to higher episode numbers by default
  . oldestFirst

  where oldestFirst = sortOn UpstreamRSSFeed.pubDate

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

downloadRSS :: MonadDownload m => URL -> m (Maybe T.Text)
downloadRSS = fmap (fmap (TE.decodeUtf8 . BL.toStrict)) . getFile

-- | Returns an upstream RSS item closest to @time@ if it's within one day.
closestUpstreamItemToTime :: UpstreamRSSFeed.PodcastId -> Connection -> UTCTime -> IO (Maybe UpstreamRSSFeed.UpstreamRSSItem)
closestUpstreamItemToTime podcast conn time = do
  r <- queryNamed conn
    "SELECT podcast,title,description,guid,publishedAt FROM episode\
    \ WHERE podcast = :podcast AND\
      \ (publishedAt BETWEEN strftime('%s', :date, '-1 days') AND strftime('%s', :date, '+1 days'))\
    \ ORDER BY abs(publishedAt - strftime('%s', :date)) ASC, publishedAt DESC\
    \ LIMIT 1"
    [":podcast" := podcast, ":date" := formatTime defaultTimeLocale "%F %T" time]
  pure $ listToMaybe r
