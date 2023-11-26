{-# LANGUAGE NoImplicitPrelude #-}
module Ripper.Types
  ( App (..)
  , HasAppOptions(..)
  , HasAppRipsQueue(..)
  , Options (..)
  , RipName
  , RipsQueue
  , StreamConfig(..)
  , StreamURL(..)
  , SuccessfulRip(..)
  , URL(..)
  ) where

import Data.Aeson (FromJSON)
import RIO
import RIO.Process
-- TODO move `Duration` outside of `RSSGen`?
import RSSGen.Duration
import Ripper.RipperDelay

newtype StreamURL = StreamURL URL
  deriving newtype (Show, Eq, FromJSON)

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  -- | The output directory for rips if set by the user.
  , optionsOutputDirectory :: !(Maybe FilePath)
  -- | Record the stream for this duration; it's used only by the `ripper` CLI.
  , optionsRipLength :: !(Maybe Duration)
  , optionsRipIntervalRefs :: ![RipperIntervalRef]
  , optionsStreamConfig :: !StreamConfig
  }

type RipName = Text

-- | The input data for the `ripper` about how to record a stream.
data StreamConfig
  -- | The config contains the stream name (to know how the live stream is
  -- checked and find out the stream URL) and the stream URL if there is no
  -- special live stream check. This is what `Podripper` uses.
  = StreamConfig !RipName !StreamURL
  -- | Contains only the stream URL, without any live checks. This simplified
  -- version should only be used by the `ripper` CLI.
  | SimpleURL !StreamURL

newtype URL = URL { urlToText :: Text }
  deriving newtype (Show, Eq, FromJSON)

-- | Stores information about a successful rip. Values of this type are passed
-- from the ripper back to the parent `Podripper` (and ultimately to the
-- reencoding step).
data SuccessfulRip = SuccessfulRip
  { ripEndTime :: !RipEndTime
  , ripFilename :: !FilePath
  }
  deriving (Eq, Show)

type RipsQueue = TQueue SuccessfulRip

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  , appUserAgent :: !Text
  , appRipsQueue :: !RipsQueue
  -- Add other app-specific configuration information here
  }

class HasAppOptions env where
  appOptionsL :: Lens' env Options

class HasAppRipsQueue env where
  appRipsQueueL :: Lens' env RipsQueue

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
instance HasAppOptions App where
  appOptionsL = lens appOptions (\x y -> x { appOptions = y })
instance HasAppRipsQueue App where
  appRipsQueueL = lens appRipsQueue (\x y -> x { appRipsQueue = y })
