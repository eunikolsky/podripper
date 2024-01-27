module RipConfig
  ( RipConfig(..)
  , RipConfigExt(..)
  , StreamURL(..)
  , extendConfig
  , loadConfig
  ) where

import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import RSSGen.Duration
import Ripper.RipperDelay
import Ripper.Types
import System.FilePath
import System.Environment
import System.Exit (die)

-- | Configuration necessary to rip a stream.
data RipConfig = RipConfig
  { streamURL :: !StreamURL
  , ripIntervalRefs :: ![RipperIntervalRef]
  , postRipEndDelays :: ![PostRipEndDelay]
  , defaultRipperDelay :: !RetryDelay
  , noDataTimeout :: !Duration
  , ripDirName :: !Text
  , podArtist :: !Text
  , podAlbum :: !Text
  }
  deriving (Show, Eq)

instance FromJSON RipConfig where
  parseJSON = withObject "RipConfig" $ \o -> RipConfig
    <$> o .: "streamURL"
    -- the key is called `ripIntervals` instead of `ripIntervalRefs` because
    -- `RipperIntervalRef` is an internal workaround for pure parsers and
    -- doesn't need to leak outside
    <*> o .: "ripIntervals"
    <*> o .: "postRipEndDelays"
    <*> o .: "defaultRipperDelay"
    <*> o .: "noDataTimeout"
    <*> o .: "ripDirName"
    <*> o .: "podArtist"
    <*> o .: "podAlbum"

data RipConfigExt = RipConfigExt
  { config :: !RipConfig
  , rawRipDir :: !FilePath
  -- ^ The output directory for raw rips recorded by ripper.
  , cleanRipDir :: !FilePath
  -- ^ The output directory for clean rips recorded by ripper.
  , trashRawRipDir :: !FilePath
  -- ^ The "trash" directory for raw rips for easier debugging.
  , doneRipDir :: !FilePath
  , doneBaseDir :: !FilePath
  -- ^ The base directory for complete rips; this should be mounted from S3.
  }

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

extendConfig :: RipConfig -> RipConfigExt
extendConfig config =
  let ripDir = T.unpack $ ripDirName config
      cleanRipDir = ripDir
      rawRipDir = cleanRipDir </> "raw"
      trashRawRipDir = cleanRipDir </> "trash"
      doneBaseDir = "complete"
      doneRipDir = doneBaseDir </> ripDir
  in RipConfigExt{config, rawRipDir, cleanRipDir, trashRawRipDir, doneRipDir, doneBaseDir}
