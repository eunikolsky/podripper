{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Podripper
  ( run
  ) where

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.List (isSuffixOf)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.OrdinalDate
import RIO (whenM, IsString, threadDelay)
import RSSGen.Duration
import qualified RSSGen.Main as RSSGen (run)
import RipConfig
import Ripper.ATPLiveStreamCheck
import qualified Ripper.Main as Ripper (run)
import qualified Ripper.Types as Ripper
import Ripper.Util
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

data RipConfigExt = RipConfigExt
  { config :: !RipConfig
  , rawRipDir :: !FilePath
  , doneRipDir :: !FilePath
  , doneBaseDir :: !FilePath
  }

run :: Ripper.RipName -> IO ()
run ripName = do
  skipRipping <- getSkipRipping
  config <- loadConfig ripName
  let configExt = extendConfig config
  ensureDirs configExt
  -- | the flag shows whether the live stream check has returned success since
  -- the start; we don't need to ask it anymore after that
  maybeStreamURL <- waitForStream config
  unless skipRipping $ maybe (pure ()) (rip configExt) maybeStreamURL
  reencodePreviousRips configExt
  reencodeRips configExt
  updateRSS configExt

ensureDirs :: RipConfigExt -> IO ()
ensureDirs RipConfigExt{rawRipDir, doneRipDir} = do
  let createParents = True
      ensureDir = createDirectoryIfMissing createParents
  forM_ [rawRipDir, doneRipDir] ensureDir

waitForStream :: RipConfig -> IO (Maybe StreamURL)
waitForStream config =
  let ripName = ripDirName config
      originalStreamURL = streamURL config
  -- the `atp` support is hardcoded in the program because its live stream check
  -- is more complicated and the stream URL needs to be extracted from the
  -- status endpoint
  -- FIXME support this via the config file
  in if ripName == "atp" then
    toProcessReady <$> runFor
      (retryDelay config)
      (duration config)
      (fromProcessReady <$> checkATPLiveStream originalStreamURL)
    else pure $ Just originalStreamURL

rip :: RipConfigExt -> StreamURL -> IO ()
rip RipConfigExt{config, rawRipDir} (StreamURL url) =
  let options = Ripper.Options
        { Ripper.optionsVerbose = True
        , Ripper.optionsOutputDirectory = Just rawRipDir
        , Ripper.optionsRipLength = duration config
        , Ripper.optionsRipIntervalRefs = ripIntervalRefs config
        , Ripper.optionsStreamConfig = Ripper.StreamConfig (ripDirName config) url
        }
  -- note: this loop is not needed on its own because the ripper should already
  -- run for `durationSec`; however, this is a guard to restart it in case it
  -- throws an exception, so `catchExceptions` is required
  in runFor (retryDelay config) (duration config) $ do
    putStrLn "starting the ripper"
    catchExceptions $ Ripper.run options

-- | Defines whether a step in the workflow is done and ready with some data `r`.
data ProcessReadiness r = NotReady | Ready r
  deriving (Eq, Show)

fromProcessReady :: Maybe r -> ProcessReadiness r
fromProcessReady (Just r) = Ready r
fromProcessReady Nothing = NotReady

toProcessReady :: ProcessReadiness r -> Maybe r
toProcessReady (Ready r) = Just r
toProcessReady NotReady = Nothing

-- | Defines the types that can represent process readiness flag, basically
-- `ProcessReadiness`, or `()` for cases when process readiness is meaningless.
-- The whole point of this typeclass is `showReadiness`, which returns `Nothing`
-- for `()` so that it's not logged! This is probably an overkill for
-- such a small use case and should be removed when/if process readiness logging
-- is removed, and there is no observable difference between the two instances.
class ProcessReadinessType a where
  mkNotReady :: a
  readinessIsReady :: a -> Bool
  showReadiness :: a -> Maybe String

instance Show r => ProcessReadinessType (ProcessReadiness r) where
  mkNotReady = NotReady
  readinessIsReady = isJust . toProcessReady
  showReadiness = Just . show

instance ProcessReadinessType () where
  mkNotReady = ()
  readinessIsReady = const False
  showReadiness = const Nothing

-- | Runs the given IO action repeatedly for the provided `duration` until it
-- returns the `Ready` state, with the `retryDelay` between each invocation.
-- If it isn't `Ready` until the `duration` expires, the final state is what
-- the action returns (that is, `NotReady`).
runFor :: ProcessReadinessType r => RetryDelay -> Duration -> IO r -> IO r
runFor retryDelay duration io = do
  now <- getCurrentTime
  let endTime = addUTCTime (toNominalDiffTime duration) now
  putStrLn $ mconcat ["now ", show now, " + ", show duration, " = end time ", show endTime]
  go endTime

  where
    go endTime = do
      now <- getCurrentTime
      let canRun = now < endTime
      putStrLn $ mconcat ["now ", show now, "; can run: ", show canRun]

      if canRun then do
        processReadiness <- io

        afterIO <- getCurrentTime
        let nextNow = addUTCTime (toNominalDiffTime $ toDuration retryDelay) afterIO
        let haveEnoughTimeForNextIteration = nextNow < endTime
        putStrLn . mconcat $
          [ "now ", show afterIO
          , "; have enough time for next iteration: ", show haveEnoughTimeForNextIteration
          ]
          <> maybe [] (\s -> ["; process readiness: ", s]) (showReadiness processReadiness)

        -- TODO how to split these two different concerns: wait until time and
        -- wait until ready?
        if readinessIsReady processReadiness then pure processReadiness
        else if haveEnoughTimeForNextIteration then do
          -- FIXME the same implementation as in `MonadTime`
          threadDelay . toMicroseconds . toDuration $ retryDelay
          go endTime
        -- TODO is it possible to simplify the implementation? there are too
        -- many return points here
        else pure processReadiness

      -- didn't manage to get the ready status before out-of-time => not ready
      else pure mkNotReady

-- | Catches synchronous exceptions (most importantly, IO exceptions) from the
-- given IO action so that they don't crash the program (this should emulate the
-- `_ || true` behavior in the former shell script). Interruption via `Ctrl+c`
-- (along with other "asynchronous exceptions") is propagated further normally
-- so that the program can be interrupted that way.
catchExceptions :: IO () -> IO ()
catchExceptions = handle $ \(e :: SomeException) ->
  -- based on https://stackoverflow.com/questions/38032076/catching-every-exception-except-for-asyncexception/38032304#38032304
  case asyncExceptionFromException @AsyncException e of
    Just ae -> do
      putStrLn $ mconcat ["async exception ", show ae, "; propagating"]
      throw ae
    Nothing ->
      putStrLn $ mconcat ["operation failed: ", show e]

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
      code <- ffmpeg ffmpegArgs ripName
      if code == ExitSuccess
        then removeFile ripName
        else do
          putStrLn $ mconcat ["reencoding ", ripName, " failed; moving the source"]
          whenM (doesFileExist reencodedRip) $ removeFile reencodedRip
          renameFile ripName $ doneRipDir </> takeBaseName ripName <> sourceRipSuffix <.> "mp3"

podTitleFromFilename :: FilePath -> IO String
podTitleFromFilename name = fromMaybe "" <$> readCommand
  -- FIXME replace with a native Haskell solution
  "sed"
  ["-nE", "s/.*([0-9]{4})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2}).*/\\1-\\2-\\3 \\4:\\5:\\6/p"]
  name

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
      code <- ffmpeg ffmpegArgs ripName
      if code == ExitSuccess
        then removeFile ripName
        else do
          putStrLn $ mconcat ["reencoding ", ripName, " failed again; leaving as is for now"]
          whenM (doesFileExist reencodedRip) $ removeFile reencodedRip

ffmpeg :: [String] -> String -> IO ExitCode
ffmpeg args ripName = do
  let stdin = ""
  (code, out, err) <- readProcessWithExitCode "ffmpeg" args stdin
  unless (null out) $ putStrLn $ mconcat ["[ffmpeg ", ripName, "] stdout: ", out]
  unless (null err) $ putStrLn $ mconcat ["[ffmpeg ", ripName, "] stderr: ", err]
  pure code

updateRSS :: RipConfigExt -> IO ()
updateRSS RipConfigExt{config, doneBaseDir} = RSSGen.run rssName
  -- FIXME replace `ripDirName` with the requested rip name and remove the field
  where rssName = NE.singleton $ doneBaseDir </> T.unpack (ripDirName config) <.> "rss"

-- | Checks for the (legacy) `END_TIMESTAMP` environment variable: the value of
-- `0` means "skip the ripping part"; all other values aren't supported at the
-- moment and will terminate the program.
getSkipRipping :: IO Bool
getSkipRipping = do
  maybeEndTimestamp <- lookupEnv "END_TIMESTAMP"
  case maybeEndTimestamp of
    Nothing -> pure False
    Just "0" -> pure True
    Just x -> do
      die $ mconcat ["END_TIMESTAMP envvar: only value `0` is supported, ", show x, " given; terminating"]

extendConfig :: RipConfig -> RipConfigExt
extendConfig config =
  let
      -- | The output directory for raw rips recorded by ripper.
      rawRipDir = T.unpack $ ripDirName config
      -- | The base directory for complete rips; this should be mounted from S3.
      doneBaseDir = "complete"
      doneRipDir = doneBaseDir </> rawRipDir
  in RipConfigExt{config, rawRipDir, doneRipDir, doneBaseDir}
