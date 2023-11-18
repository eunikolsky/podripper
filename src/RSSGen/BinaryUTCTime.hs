{-# OPTIONS_GHC -Wno-orphans #-}

module RSSGen.BinaryUTCTime () where

import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock.Compat ()
import Development.Shake.Classes

-- this is required for `shake`'s oracles
instance Binary UTCTime where
  get = do
    (year, dayOfYear) <- get
    let utctDay = fromOrdinalDate year dayOfYear
    utctDayTime <- picosecondsToDiffTime <$> get
    pure UTCTime{utctDay, utctDayTime}

  put UTCTime{utctDay, utctDayTime} = do
    let (year, dayOfYear) = toOrdinalDate utctDay
    put (year, dayOfYear)
    put . diffTimeToPicoseconds $ utctDayTime
