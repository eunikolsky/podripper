{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Podripper
  ( run
  ) where

import Conduit
import Control.Exception (AsyncException, throw)
import Control.Monad
import Data.Conduit.List qualified as C
import Data.Maybe
import Data.List (isSuffixOf)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.OrdinalDate
import MP3.ID3
import MP3.Xing
import RIO hiding (stdin)
import RSSGen.Duration
import qualified RSSGen.Main as RSSGen (run)
import RipConfig
import qualified Ripper.Main as Ripper (run)
import qualified Ripper.Types as Ripper
import Ripper.Util
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

run :: Ripper.RipName -> IO ()
run ripName = do
  skipRipping <- getSkipRipping
  config <- loadConfig ripName
  let configExt = extendConfig config
  ensureDirs configExt

  ripsQueue <- atomically newTQueue
  reencodedQueue <- atomically newTQueue

  let doRip = rip ripsQueue configExt
      doProcessSuccessfulRips = processSuccessfulRips configExt ripsQueue reencodedQueue
      doProcessReencodedRips = processReencodedRips configExt reencodedQueue
      -- reencode discovered leftover rips, if any, in parallel with ripping at
      -- the startup
      doReencodePreviousRips = reencodePreviousRips configExt reencodedQueue
      terminateReencodedQueue = atomically $ writeTQueue reencodedQueue QFinish
      doInitialRSSUpdate = atomically $ writeTQueue reencodedQueue $ QValue InitialRSSUpdate

  if skipRipping
    -- when `skipRipping`, there is no endless ripping loop, so we need to
    -- process whatever unprocesses rips are found and quit cleanly; that's why
    -- `concurrently_` is used: to wait until all is done
    then concurrently_
      (doReencodePreviousRips >> terminateReencodedQueue)
      doProcessReencodedRips
    -- `concurrently_` is used here because the fast `doReencodePreviousRips`
    -- shouldn't terminate the other concurrent processes
    else concurrently_
      doReencodePreviousRips
      -- `race` waits until any process finishes (which they don't here), and
      -- also terminates everything if any one throws an exception
      $ race3
          doRip
          doProcessSuccessfulRips
          -- do an initial RSS update in case any new rip comes up: this may
          -- happen in the scenario when the ripper has recorded a rip, it was
          -- reencoded and the `rssgen` was waiting for upstream feed updates
          -- when the process was killed; if there are no changes, the RSS won't
          -- be regenerated
          (doInitialRSSUpdate >> doProcessReencodedRips)

race3 :: IO a -> IO b -> IO c -> IO ()
race3 x y = race_ x . race_ y

processSuccessfulRips :: RipConfigExt -> Ripper.RipsQueue -> ReencodedQueue -> IO ()
processSuccessfulRips config queue reencodedQueue = forever $ do
  newRip <- atomically $ readTQueue queue
  putStrLn $ "Successful rip: " <> show newRip
  reencodeRip config newRip
  atomically $ writeTQueue reencodedQueue $ QValue NewReencodedRip

-- | An event in a `TerminatableQueue`: either a value or a termination signal.
data QEvent a = QValue a | QFinish

-- TODO use `Control.Concurrent.STM.TMQueue` instead?
-- | A `TQueue` that sends data and can also send a termination signal.
type TerminatableQueue a = TQueue (QEvent a)

-- | Events in the `ReencodedQueue`. This type exists only for the logging, so
-- that the initial RSS update doesn't print "New reencoded rip", which is
-- incorrect.
data ReencodedEvent = NewReencodedRip | InitialRSSUpdate

-- | A queue of reencoded rips sends only empty values because the RSS updater
-- lists and uses all the available files in the complete directory anyway. A
-- reason to send the reencoded filename could be to point the updater to the
-- newest file, but I'm not sure how to do that with `shake`, which needs only
-- the target filename to produce.
type ReencodedQueue = TerminatableQueue ReencodedEvent

processReencodedRips :: RipConfigExt -> ReencodedQueue -> IO ()
processReencodedRips config queue = go
  where
    go = do
      event <- atomically $ readTQueue queue
      case event of
        QValue reencodedEvent -> do
          putStrLn $ reencodedEventDescription reencodedEvent
          updateRSS config
          go

        QFinish -> pure ()

    reencodedEventDescription :: ReencodedEvent -> String
    reencodedEventDescription NewReencodedRip = "New reencoded rip"
    reencodedEventDescription InitialRSSUpdate = "Initial RSS update"

ensureDirs :: RipConfigExt -> IO ()
ensureDirs RipConfigExt{rawRipDir, trashRawRipDir, doneRipDir} = do
  let createParents = True
      ensureDir = createDirectoryIfMissing createParents
  forM_ [rawRipDir, trashRawRipDir, doneRipDir] ensureDir

rip :: Ripper.RipsQueue -> RipConfigExt -> IO ()
rip ripsQueue RipConfigExt{config, rawRipDir, cleanRipDir} =
  let options = Ripper.Options
        { Ripper.optionsVerbose = True
        , Ripper.optionsRawRipsDirectory = Just rawRipDir
        , Ripper.optionsCleanRipsDirectory = Just cleanRipDir
        , Ripper.optionsRipLength = Nothing
        , Ripper.optionsRipIntervalRefs = ripIntervalRefs config
        , Ripper.optionsPostRipEndDelays = postRipEndDelays config
        , Ripper.optionsDefaultRipDelay = defaultRipperDelay config
        , Ripper.optionsNoDataTimeout = noDataTimeout config
        , Ripper.optionsStreamConfig = Ripper.StreamConfig (ripDirName config) (streamURL config)
        }
  -- note: this loop is not needed on its own because the ripper should already
  -- run for `durationSec`; however, this is a guard to restart it in case it
  -- throws an exception, so `catchExceptions` is required
  in forever $ do
    putStrLn "starting the ripper"
    catchExceptions $ Ripper.run options ripsQueue
    -- TODO handle most HTTP exceptions inside the ripper?
    threadDelay . toMicroseconds $ durationSeconds 1

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

reencodeRip :: RipConfigExt -> Ripper.SuccessfulRip -> IO ()
reencodeRip configExt@RipConfigExt{config, doneRipDir} newRip = do
  -- TODO get year from the file itself
  year <- show . fst . toOrdinalDate . localDay . zonedTimeToLocalTime <$> getZonedTime
  reencodeRip' year newRip

  where
    reencodeRip' :: String -> Ripper.SuccessfulRip -> IO ()
    reencodeRip' year Ripper.SuccessfulRip{Ripper.ripFilename=ripName, Ripper.ripMP3Structure=mp3} = do
      podTitle <- podTitleFromFilename ripName
      let reencodedRip = reencodedRipNameFromOriginal doneRipDir ripName
          id3Header = getID3Header . generateID3Header $ ID3Fields
            { id3Title = T.pack podTitle
            , id3Artist = podArtist config
            , id3Album = podAlbum config
            -- TODO can actually use a more precise timestamp now
            , id3Date = T.pack year
            , id3Genre = "Podcast"
            }
          xingHeader = getXingHeader $ calculateXingHeader mp3

      runConduitRes $
        C.sourceList [id3Header, xingHeader] *> sourceFile ripName
        .| sinkFile reencodedRip
      trashFile configExt ripName

-- FIXME clean 2+ weeks old files
trashFile :: RipConfigExt -> FilePath -> IO ()
trashFile RipConfigExt{trashRawRipDir} file = do
  let targetFile = trashRawRipDir </> takeFileName file
  putStrLn $ mconcat ["Moving ", file, " to ", targetFile]
  renameFile file targetFile

podTitleFromFilename :: FilePath -> IO String
podTitleFromFilename name = fromMaybe "" <$> readCommand
  -- FIXME replace with a native Haskell solution
  "sed"
  ["-nE", "s/.*([0-9]{4})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2}).*/\\1-\\2-\\3 \\4:\\5:\\6/p"]
  name

reencodedRipNameFromOriginal :: FilePath -> FilePath -> FilePath
reencodedRipNameFromOriginal doneRipDir ripName = doneRipDir </> takeBaseName ripName <> reencodedRipSuffix <.> "mp3"

{- |
 - discover previously failed to convert rips and try to reencode them again [0]
 - and also discover and reencode original rips in the source dir, which may be
 - there as a result of a crash
 -
 - [0] for example, this helps with the case when `ffmpeg` after an update fails
 - to run (`GLIBC` ld error) and reencode the fresh rips, then a fix comes and
 - we can try reencoding those older ones again
 -}
reencodePreviousRips :: RipConfigExt -> ReencodedQueue -> IO ()
reencodePreviousRips configExt@RipConfigExt{config, doneRipDir, rawRipDir} queue = do
  ripSources <- fmap (doneRipDir </>) . filter previouslyFailedRip <$> listDirectory doneRipDir
  ripOriginals <- fmap (rawRipDir </>) . filter isMP3 <$> listDirectory rawRipDir
  let ripsSources' =
        -- FIXME parse the MP3 structure from the file
        (\ripName -> (Ripper.SuccessfulRip ripName (MP3Structure mempty), T.unpack . T.replace sourceRipSuffix reencodedRipSuffix . T.pack $ ripName))
          <$> ripSources
      ripOriginals' =
        (\ripName -> (Ripper.SuccessfulRip ripName (MP3Structure mempty), reencodedRipNameFromOriginal doneRipDir ripName))
          <$> ripOriginals
      rips = ripsSources' <> ripOriginals'
  -- TODO get year from the file itself
  year <- show . fst . toOrdinalDate . localDay . zonedTimeToLocalTime <$> getZonedTime
  forM_ rips (reencodeRip' year)

  where
    isMP3 = (== ".mp3") . takeExtension
    previouslyFailedRip f = all ($ f)
      [isMP3, (sourceRipSuffix `isSuffixOf`) . takeBaseName]

    reencodeRip' :: String -> (Ripper.SuccessfulRip, FilePath) -> IO ()
    reencodeRip' year (Ripper.SuccessfulRip{Ripper.ripFilename=ripName}, reencodedRip) = do
      podTitle <- podTitleFromFilename ripName
      let ffmpegArgs =
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
        then do
          trashFile configExt ripName
          atomically $ writeTQueue queue $ QValue NewReencodedRip
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
  where rssName = doneBaseDir </> T.unpack (ripDirName config) <.> "rss"

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
