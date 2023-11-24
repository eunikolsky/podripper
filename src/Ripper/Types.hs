{-# LANGUAGE NoImplicitPrelude #-}
module Ripper.Types
  ( App (..)
  , HasAppOptions(..)
  , Options (..)
  ) where

import RIO
import RIO.Process
-- TODO move `Duration` outside of `RSSGen`?
import RSSGen.Duration
import Ripper.RipperDelay

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  -- | The output directory for rips if set by the user.
  , optionsOutputDirectory :: !(Maybe FilePath)
  -- | Record the stream for this duration.
  , optionsRipLength :: !Duration
  , optionsRipIntervalRefs :: ![RipperIntervalRef]
  -- | Delay for this duration before trying to reconnect when there were no
  -- recordings yet, i.e. the stream hasn't started yet.
  , optionsReconnectDelay :: !RetryDelay
  -- | Delay for this duration before trying to reconnect since there was a
  -- recording, i.e. either the stream is live and there was a disconnect, or
  -- the stream has ended.
  , optionsSmallReconnectDelay :: !RetryDelay
  , optionsStreamURL :: !Text
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  , appUserAgent :: !Text
  -- Add other app-specific configuration information here
  }

class HasAppOptions env where
  appOptionsL :: Lens' env Options

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
instance HasAppOptions App where
  appOptionsL = lens appOptions (\x y -> x { appOptions = y })
