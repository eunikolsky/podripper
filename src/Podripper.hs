module Podripper
  ( RipName
  , main
  ) where

import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import RipConfig
import System.Directory
import System.Environment
import System.Exit (die, exitFailure)
import System.FilePath

type RipName = Text

main :: RipName -> IO ()
main ripName = do
  config <- loadConfig ripName
  ensureDirs config
  -- FIXME remove `exitFailure` when the script has been migrated
  exitFailure

loadConfig :: RipName -> IO RipConfig
loadConfig ripName = do
  confDir <- getConfDir
  let confName = confDir </> T.unpack ripName <.> "json"
  eitherConfig <- eitherDecodeFileStrict' @RipConfig confName
  either die pure eitherConfig

ensureDirs :: RipConfig -> IO ()
ensureDirs conf = do
  let createParents = True
      ensureDir = createDirectoryIfMissing createParents
      -- | The output directory for raw rips recorded by ripper.
      rawRipDir = T.unpack $ ripDirName conf
      -- | The base directory for complete rips; this should be mounted from S3.
      doneBaseDir = "complete"
      doneRipDir = doneBaseDir </> rawRipDir
  forM_ [rawRipDir, doneRipDir] ensureDir

getConfDir :: IO FilePath
getConfDir = do
  maybeConfDir <- lookupEnv "CONF_DIR"
  pure $ fromMaybe "/usr/share/podripper" maybeConfDir
