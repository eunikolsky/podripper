{-# LANGUAGE NoImplicitPrelude #-}
module Types
  ( App (..)
  , Options (..)
  ) where

import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  -- | The output directory for rips if set by the user.
  , optionsOutputDirectory :: !(Maybe FilePath)
  , optionsStreamURL :: !Text
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
