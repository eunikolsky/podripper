{-# LANGUAGE NoImplicitPrelude #-}
module Ripper.Types
  ( App (..)
  , HasAppOptions(..)
  , Options (..)
  , URL(..)
  ) where

import Data.Aeson (FromJSON)
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
  , optionsStreamURL :: !URL
  }

newtype URL = URL { urlToText :: Text }
  deriving newtype (Show, Eq, FromJSON)

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
