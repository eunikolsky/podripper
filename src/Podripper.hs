module Podripper
  ( RipName
  , main
  ) where

import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import RipConfig
import System.Environment
import System.Exit (die)
import System.FilePath

type RipName = Text

main :: RipName -> IO ()
main ripName = do
  config <- loadConfig ripName
  -- FIXME remove `die` when the script has been migrated
  die $ "Loaded config: " <> show config

loadConfig :: RipName -> IO RipConfig
loadConfig ripName = do
  confDir <- getConfDir
  let confName = confDir </> T.unpack ripName <.> "json"
  eitherConfig <- eitherDecodeFileStrict' @RipConfig confName
  either die pure eitherConfig

getConfDir :: IO FilePath
getConfDir = do
  maybeConfDir <- lookupEnv "CONF_DIR"
  pure $ fromMaybe "/usr/share/podripper" maybeConfDir
