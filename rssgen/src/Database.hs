{-# LANGUAGE OverloadedStrings #-}

module Database
  ( closestUpstreamItemToTime
  , openDatabase
  , saveUpstreamRSSItems
  ) where

import Database.SQLite.Simple
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Time.Clock
import Data.Time.Format

import qualified UpstreamRSSFeed

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
