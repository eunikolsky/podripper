{-# LANGUAGE OverloadedStrings #-}

module RSSGen.Database
  ( DBConnection
  , FileSpec(..)

  -- * database metafunctions
  , openDatabase
  , closeDatabase
  , withDatabase

  -- * working with upstream RSS
  , closestUpstreamItemToTime
  , saveUpstreamRSSItems

  -- * persisting download cache items
  , getCacheItem
  , setCacheItem
  ) where

import Control.Exception
import Database.SQLite.Simple
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Time.Clock
import Data.Time.Format

import RSSGen.DownloaderTypes
import RSSGen.Duration
import qualified RSSGen.UpstreamRSSFeed as UpstreamRSSFeed

-- | Type alias for the database's `Connection` to have a more descriptive type.
type DBConnection = Connection

data FileSpec
  = DefaultFile -- ^ The default @episodes.sqlite@ file
  | InMemory    -- ^ An in-memory database

-- | Returns the SQLite database string for the given file spec.
dbFileName :: FileSpec -> String
dbFileName DefaultFile = "episodes.sqlite"
dbFileName InMemory    = ""

-- | Executes the given `IO` action with an exception-safe access to the
-- database.
withDatabase :: FileSpec -> (DBConnection -> IO a) -> IO a
-- note: using `bracket` instead of `withConnection` because I still have to
-- export `{open,close}Database` functions and want to use them here
withDatabase f = bracket (openDatabase f) closeDatabase

-- |Opens the episodes database and ensures the necessary tables are created.
openDatabase :: FileSpec -> IO Connection
openDatabase fileSpec = do
  conn <- open $ dbFileName fileSpec
  mapM_ (execute_ conn) [createEpisodeTable, createHTTPCacheTable]
  pure conn

  where
    createEpisodeTable =
      "CREATE TABLE IF NOT EXISTS episode (\
      \ id INTEGER PRIMARY KEY ASC NOT NULL,\
      \ podcast TEXT NOT NULL,\
      \ title TEXT NOT NULL,\
      \ description TEXT NOT NULL,\
      \ guid TEXT NOT NULL,\
      \ publishedAt INTEGER NOT NULL,\
      \ addedAt INTEGER NOT NULL DEFAULT (strftime('%s', 'now')),\
      \ UNIQUE (podcast, title, description, guid, publishedAt) ON CONFLICT IGNORE)"

    -- `either_etag_lastmod_or_body`: we want to have either ETag and/or
    -- Last-Modified, or response body, but not both, the not equal operator
    -- `<>` works like XOR in this case
    -- (https://sqlite-users.sqlite.narkive.com/hb6ya107/sqlite-xor-operator)
    --
    -- `lastModified`: even though we know that this is a time, we don't need to
    -- do anything with it except for sending it back to the server later (just
    -- like `eTag`), so it's reasonable to treat it as an opaque blob
    --
    -- `modifiedAt`: the default `now` timestamp applies to adding and replacing data
    createHTTPCacheTable =
      "CREATE TABLE IF NOT EXISTS httpCache (\
      \ id INTEGER PRIMARY KEY ASC NOT NULL,\
      \ url TEXT NOT NULL,\
      \ eTag BLOB,\
      \ lastModified BLOB,\
      \ body BLOB,\
      \ modifiedAt INTEGER NOT NULL DEFAULT(strftime('%s', 'now')),\
      \ CONSTRAINT either_etag_lastmod_or_body\
        \ CHECK ( (eTag NOTNULL OR lastModified NOTNULL) <> (body NOTNULL) ),\
      \ UNIQUE (url) ON CONFLICT REPLACE);"

closeDatabase :: Connection -> IO ()
closeDatabase = close

-- |Saves all the parsed upstream RSS items into the database. The `episode`
-- |table ignores duplicate records.
saveUpstreamRSSItems :: Connection -> [UpstreamRSSFeed.UpstreamRSSItem] -> IO ()
saveUpstreamRSSItems conn
  = executeMany conn "INSERT INTO episode (podcast,title,description,guid,publishedAt) VALUES (?,?,?,?,?)"
  -- this provides a small benefit of the initially imported items being
  -- sorted from lower to higher episode numbers by default
  . oldestFirst

  where oldestFirst = sortOn UpstreamRSSFeed.pubDate

-- | Returns an upstream RSS item closest to @time@ if it's within the specified
-- `duration`.
closestUpstreamItemToTime :: Duration -> UpstreamRSSFeed.PodcastId -> Connection -> UTCTime -> IO (Maybe UpstreamRSSFeed.UpstreamRSSItem)
closestUpstreamItemToTime duration podcast conn time = do
  r <- queryNamed conn
    "SELECT podcast,title,description,guid,publishedAt FROM episode\
    \ WHERE podcast = :podcast AND\
      \ (publishedAt BETWEEN strftime('%s', :date, '-' || :seconds || ' seconds') AND strftime('%s', :date, '+' || :seconds || ' seconds'))\
    \ ORDER BY abs(publishedAt - strftime('%s', :date)) DESC, publishedAt DESC\
    \ LIMIT 1"
    [ ":podcast" := podcast
    , ":date" := formatTime defaultTimeLocale "%F %T" time
    , ":seconds" := toSeconds duration
    ]
  pure $ listToMaybe r

setCacheItem :: Connection -> URL -> CacheItem -> IO ()
setCacheItem conn url (ETag etag) = executeNamed conn
  "INSERT INTO httpcache (url,etag) VALUES (:url,:etag)"
  [":url" := url, ":etag" := etag]
setCacheItem conn url (LastModified lastmod) = executeNamed conn
  "INSERT INTO httpcache (url,lastModified) VALUES (:url,:lastmod)"
  [":url" := url, ":lastmod" := lastmod]
setCacheItem conn url (ETagWithLastModified etag lastmod) = executeNamed conn
  "INSERT INTO httpcache (url,etag,lastModified) VALUES (:url,:etag,:lastmod)"
  [":url" := url, ":etag" := etag, ":lastmod" := lastmod]
setCacheItem conn url (Body body) = executeNamed conn
  "INSERT INTO httpcache (url,body) VALUES (:url,:body)"
  [":url" := url, ":body" := body]

getCacheItem :: Connection -> URL -> IO (Maybe CacheItem)
getCacheItem conn url = fmap fromDatabaseCacheItem . listToMaybe <$> queryNamed conn
  "SELECT etag,lastModified,body FROM httpCache WHERE url = :url"
  [":url" := url]

-- | This type wraps the raw result of retrieving a cache item. It's needed
-- because `FromRow CacheItem` itself can't distinguish between ETag and
-- LastModified since one of them could be `null`.
data DatabaseCacheItem = DatabaseCacheItem
  (Maybe {-ETag-} Bytes)
  (Maybe {-LastModified-} Bytes)
  (Maybe {-Body-} Bytes)
  deriving Show

instance FromRow DatabaseCacheItem where
  fromRow = DatabaseCacheItem <$> field <*> field <*> field

fromDatabaseCacheItem :: DatabaseCacheItem -> CacheItem
fromDatabaseCacheItem (DatabaseCacheItem (Just etag) Nothing        Nothing    ) = ETag etag
fromDatabaseCacheItem (DatabaseCacheItem Nothing     (Just lastmod) Nothing    ) = LastModified lastmod
fromDatabaseCacheItem (DatabaseCacheItem (Just etag) (Just lastmod) Nothing    ) = ETagWithLastModified etag lastmod
fromDatabaseCacheItem (DatabaseCacheItem Nothing     Nothing        (Just body)) = Body body
fromDatabaseCacheItem i = error $ "fromDatabaseCacheItem: impossible case " <> show i
