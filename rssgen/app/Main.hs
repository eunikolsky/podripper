{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Ord (Down(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Time.Clock
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

          liftIO $ close conn
          generateFeed config (fromMaybe [] maybeUpstreamRSS) out
        Nothing -> fail $ "Couldn't parse feed config file " <> feedConfigFile

    where
      generateFeed :: RSSFeedConfig -> [UpstreamRSSFeed.UpstreamRSSItem] -> FilePattern -> Action ()
      generateFeed feedConfig upstreamItems out = do
        -- we need the audio files to generate the RSS, which are in the
        -- directory of the same name as the podcast title
        let podcastTitle = dropExtension out
        mp3Files <- getDirectoryFiles "" [podcastTitle </> "*.mp3"]
        let findUpstreamItem = closestUpstreamItemToTime upstreamItems
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
    "CREATE TABLE episode (\
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

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

downloadRSS :: MonadDownload m => URL -> m (Maybe T.Text)
downloadRSS = fmap (fmap (TE.decodeUtf8 . BL.toStrict)) . getFile

-- | Returns an upstream RSS item closest to @time@ if it's within one day.
closestUpstreamItemToTime :: [UpstreamRSSFeed.UpstreamRSSItem] -> UTCTime -> Maybe UpstreamRSSFeed.UpstreamRSSItem
closestUpstreamItemToTime items time = do
  let closeItems = filter (withinOneDay time) items
  guardNonEmpty closeItems
  return $ maximumBy (compare `on` UpstreamRSSFeed.pubDate) closeItems

  where
    withinOneDay :: UTCTime -> UpstreamRSSFeed.UpstreamRSSItem -> Bool
    withinOneDay time item = (< nominalDay) . abs . diffUTCTime time $ UpstreamRSSFeed.pubDate item

    guardNonEmpty :: Alternative f => [a] -> f ()
    guardNonEmpty = guard . not . null
