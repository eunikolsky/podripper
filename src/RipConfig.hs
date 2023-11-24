module RipConfig
  ( RipConfig(..)
  , RipName
  , StreamURL(..)
  , loadConfig
  ) where

import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import RSSGen.Duration
import Ripper.RipperDelay
import System.FilePath
import System.Environment
import System.Exit (die)

newtype StreamURL = StreamURL Text
  deriving newtype (Show, Eq, FromJSON)

-- | Configuration necessary to rip a stream.
data RipConfig = RipConfig
  { streamURL :: !StreamURL
  -- FIXME remove `duration` when endless ripping is finished
  , duration :: !Duration
  -- FIXME remove `retryDelay` when live check uses rip intervals too
  , retryDelay :: !RetryDelay
  , ripIntervalRefs :: ![RipperIntervalRef]
  , ripDirName :: !Text
  , podArtist :: !Text
  , podAlbum :: !Text
  }
  deriving (Show, Eq)

instance FromJSON RipConfig where
  parseJSON = withObject "RipConfig" $ \o -> RipConfig
    <$> o .: "streamURL"
    <*> o .: "duration"
    <*> o .: "retryDelay"
    -- the key is called `ripIntervals` instead of `ripIntervalRefs` because
    -- `RipperIntervalRef` is an internal workaround for pure parsers and
    -- doesn't need to leak outside
    <*> o .: "ripIntervals"
    <*> o .: "ripDirName"
    <*> o .: "podArtist"
    <*> o .: "podAlbum"

type RipName = Text

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
