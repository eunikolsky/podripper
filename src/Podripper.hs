module Podripper
  ( RipName
  , main
  ) where

import Control.Monad
import Data.Aeson
import Data.Maybe
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified RSSGen.Main as RSSGen (main)
import RipConfig
import qualified Ripper.Main as Ripper (main)
import qualified Ripper.Types as Ripper (Options(..))
import System.Directory
import System.Environment
import System.Exit (die, exitFailure)
import System.FilePath

type RipName = Text

data RipConfigExt = RipConfigExt
  { config :: !RipConfig
  , rawRipDir :: !FilePath
  , doneRipDir :: !FilePath
  , doneBaseDir :: !FilePath
  }

main :: RipName -> IO ()
main ripName = do
  config <- loadConfig ripName
  let configExt = extendConfig config
  ensureDirs configExt
  rip configExt
  updateRSS configExt
  -- FIXME remove `exitFailure` when the script has been migrated
  exitFailure

loadConfig :: RipName -> IO RipConfig
loadConfig ripName = do
  confDir <- getConfDir
  let confName = confDir </> T.unpack ripName <.> "json"
  eitherConfig <- eitherDecodeFileStrict' @RipConfig confName
  either die pure eitherConfig

ensureDirs :: RipConfigExt -> IO ()
ensureDirs RipConfigExt{rawRipDir, doneRipDir} = do
  let createParents = True
      ensureDir = createDirectoryIfMissing createParents
  forM_ [rawRipDir, doneRipDir] ensureDir

rip :: RipConfigExt -> IO ()
rip RipConfigExt{config, rawRipDir} = do
  putStrLn "starting the ripper"
  let options = Ripper.Options
        { Ripper.optionsVerbose = True
        , Ripper.optionsOutputDirectory = Just rawRipDir
        , Ripper.optionsRipLengthSeconds = fromIntegral $ durationSec config
        , Ripper.optionsReconnectDelay = fromIntegral $ retrySec config
        , Ripper.optionsSmallReconnectDelay = 1
        , Ripper.optionsStreamURL = streamURL config
        }
  Ripper.main options

updateRSS :: RipConfigExt -> IO ()
updateRSS RipConfigExt{config, doneBaseDir} =
  withCurrentDirectory doneBaseDir $ RSSGen.main rssName
  -- FIXME replace `ripDirName` with the requested rip name and remove the field
  where rssName = NE.singleton $ T.unpack (ripDirName config) <.> "rss"

extendConfig :: RipConfig -> RipConfigExt
extendConfig config =
  let
      -- | The output directory for raw rips recorded by ripper.
      rawRipDir = T.unpack $ ripDirName config
      -- | The base directory for complete rips; this should be mounted from S3.
      doneBaseDir = "complete"
      doneRipDir = doneBaseDir </> rawRipDir
  in RipConfigExt{config, rawRipDir, doneRipDir, doneBaseDir}

getConfDir :: IO FilePath
getConfDir = do
  maybeConfDir <- lookupEnv "CONF_DIR"
  pure $ fromMaybe "/usr/share/podripper" maybeConfDir
