module Podripper
  ( RipName
  , main
  ) where

import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.List (isSuffixOf)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.OrdinalDate
import RIO (whenM, IsString)
import qualified RSSGen.Main as RSSGen (main)
import RipConfig
import qualified Ripper.Main as Ripper (main)
import qualified Ripper.Types as Ripper (Options(..))
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

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
  reencodePreviousRips configExt
  reencodeRips configExt
  updateRSS configExt

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

sourceRipSuffix, reencodedRipSuffix :: IsString s => s
sourceRipSuffix = "_src"
reencodedRipSuffix = "_enc"

reencodeRips :: RipConfigExt -> IO ()
reencodeRips RipConfigExt{config, rawRipDir, doneRipDir} = do
  rips <- fmap (rawRipDir </>) . filter ((== ".mp3") . takeExtension) <$> listDirectory rawRipDir
  year <- show . fst . toOrdinalDate . localDay . zonedTimeToLocalTime <$> getZonedTime
  if not (null rips)
  then forM_ rips (reencodeRip year)
  else putStrLn $ "no files in " <> rawRipDir

  where
    reencodeRip year ripName = do
      podTitle <- podTitleFromFilename ripName
      let reencodedRip = doneRipDir </> takeBaseName ripName <> reencodedRipSuffix <.> "mp3"
          ffmpegArgs =
            [ "-nostdin"
            , "-hide_banner"
            , "-i", ripName
            , "-vn"
            , "-v", "warning"
            , "-codec:a", "libmp3lame"
            , "-b:a", "96k"
            , "-metadata", "title=" <> podTitle
            , "-metadata", "artist=" <> T.unpack (podArtist config)
            , "-metadata", "album=" <> T.unpack (podAlbum config)
            , "-metadata", "date=" <> year
            , "-metadata", "genre=Podcast"
            , reencodedRip
            ]
      (code, out, err) <- readProcessWithExitCode "ffmpeg" ffmpegArgs ""
      unless (null out) $ putStrLn $ mconcat ["[ffmpeg ", ripName, "] stdout: ", out]
      unless (null err) $ putStrLn $ mconcat ["[ffmpeg ", ripName, "] stderr: ", err]
      if code == ExitSuccess
        then removeFile ripName
        else do
          putStrLn $ mconcat ["reencoding ", ripName, " failed; moving the source"]
          whenM (doesFileExist reencodedRip) $ removeFile reencodedRip
          renameFile ripName $ doneRipDir </> takeBaseName ripName <> sourceRipSuffix <.> "mp3"

podTitleFromFilename :: FilePath -> IO String
podTitleFromFilename name = do
  -- FIXME replace with a native Haskell solution
  (code, out, err) <- readProcessWithExitCode "sed" ["-nE", "s/.*([0-9]{4})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2}).*/\\1-\\2-\\3 \\4:\\5:\\6/p"] name
  if code == ExitSuccess
    then pure out
    else do
      putStrLn $ mconcat ["podTitleFromFilename " <> name <> ": exit code ", show code, "; stderr: ", err]
      pure ""

{- |
 - discover previously failed to convert rips and try to reencode them again
 - (for example, this helps with the case when `ffmpeg` after an update fails
 - to run (`GLIBC` ld error) and reencode the fresh rips, then a fix comes and
 - we can try reencoding those older ones again)
 - this needs to happen before reencoding fresh rips because if those fail, they
 - would be attempted to be reencoded again in this run, which isn't very useful
 -}
reencodePreviousRips :: RipConfigExt -> IO ()
reencodePreviousRips RipConfigExt{config, doneRipDir} = do
  rips <- fmap (doneRipDir </>) . filter previouslyFailedRip <$> listDirectory doneRipDir
  year <- show . fst . toOrdinalDate . localDay . zonedTimeToLocalTime <$> getZonedTime
  forM_ rips (reencodeRip year)

  where
    previouslyFailedRip f = all ($ f)
      [ (== ".mp3") . takeExtension
      , (sourceRipSuffix `isSuffixOf`) . takeBaseName
      ]

    reencodeRip year ripName = do
      podTitle <- podTitleFromFilename ripName
      let reencodedRip = T.unpack . T.replace sourceRipSuffix reencodedRipSuffix . T.pack $ ripName
          ffmpegArgs =
            [ "-nostdin"
            , "-hide_banner"
            , "-y"
            , "-i", ripName
            , "-vn"
            , "-v", "warning"
            , "-codec:a", "libmp3lame"
            , "-b:a", "96k"
            , "-metadata", "title=" <> podTitle
            , "-metadata", "artist=" <> T.unpack (podArtist config)
            , "-metadata", "album=" <> T.unpack (podAlbum config)
            , "-metadata", "date=" <> year
            , "-metadata", "genre=Podcast"
            , reencodedRip
            ]
      (code, out, err) <- readProcessWithExitCode "ffmpeg" ffmpegArgs ""
      unless (null out) $ putStrLn $ mconcat ["[ffmpeg ", ripName, "] stdout: ", out]
      unless (null err) $ putStrLn $ mconcat ["[ffmpeg ", ripName, "] stderr: ", err]
      if code == ExitSuccess
        then removeFile ripName
        else do
          putStrLn $ mconcat ["reencoding ", ripName, " failed again; leaving as is for now"]
          whenM (doesFileExist reencodedRip) $ removeFile reencodedRip

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
