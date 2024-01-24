{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Podripper
  ( run
  ) where

import Conduit
import Control.Exception (AsyncException, throw)
import Control.Monad
import Data.Conduit.Attoparsec
import Data.Conduit.List qualified as C
import Data.Maybe
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Vector qualified as V
import MP3.ID3
import MP3.MP3
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

run :: Ripper.RipName -> IO ()
run ripName = do
  skipRipping <- getSkipRipping
  config <- loadConfig ripName
  let configExt = extendConfig config
  ensureDirs configExt

  ripsQueue <- atomically newTQueue
  processedQueue <- atomically newTQueue

  let doRip = rip ripsQueue configExt
      doProcessSuccessfulRips = processSuccessfulRips configExt ripsQueue processedQueue
      doProcessProcessedRips = processProcessedRips configExt processedQueue
      -- process discovered leftover rips, if any, in parallel with ripping at
      -- the startup
      doProcessPreviousRips = processPreviousRips configExt processedQueue
      terminateProcessedQueue = atomically $ writeTQueue processedQueue QFinish
      doInitialRSSUpdate = atomically $ writeTQueue processedQueue $ QValue InitialRSSUpdate

  if skipRipping
    -- when `skipRipping`, there is no endless ripping loop, so we need to
    -- process whatever unprocesses rips are found and quit cleanly; that's why
    -- `concurrently_` is used: to wait until all is done
    then concurrently_
      (doProcessPreviousRips >> terminateProcessedQueue)
      doProcessProcessedRips
    -- `concurrently_` is used here because the fast `doProcessPreviousRips`
    -- shouldn't terminate the other concurrent processes
    else concurrently_
      doProcessPreviousRips
      -- `race` waits until any process finishes (which they don't here), and
      -- also terminates everything if any one throws an exception
      $ race3
          doRip
          doProcessSuccessfulRips
          -- do an initial RSS update in case any new rip comes up: this may
          -- happen in the scenario when the ripper has recorded a rip, it was
          -- processed and the `rssgen` was waiting for upstream feed updates
          -- when the process was killed; if there are no changes, the RSS won't
          -- be regenerated
          (doInitialRSSUpdate >> doProcessProcessedRips)

race3 :: IO a -> IO b -> IO c -> IO ()
race3 x y = race_ x . race_ y

processSuccessfulRips :: RipConfigExt -> Ripper.RipsQueue -> ProcessedQueue -> IO ()
processSuccessfulRips config queue processedQueue = forever $ do
  newRip <- atomically $ readTQueue queue
  putStrLn $ "Successful rip: " <> show newRip
  processRip config newRip
  atomically $ writeTQueue processedQueue $ QValue NewProcessedRip

-- | An event in a `TerminatableQueue`: either a value or a termination signal.
data QEvent a = QValue a | QFinish

-- TODO use `Control.Concurrent.STM.TMQueue` instead?
-- | A `TQueue` that sends data and can also send a termination signal.
type TerminatableQueue a = TQueue (QEvent a)

-- | Events in the `ProcessedQueue`. This type exists only for the logging, so
-- that the initial RSS update doesn't print "New processed rip", which is
-- incorrect.
data ProcessedEvent = NewProcessedRip | InitialRSSUpdate

-- | A queue of processed rips sends only empty values because the RSS updater
-- lists and uses all the available files in the complete directory anyway. A
-- reason to send the processed filename could be to point the updater to the
-- newest file, but I'm not sure how to do that with `shake`, which needs only
-- the target filename to produce.
type ProcessedQueue = TerminatableQueue ProcessedEvent

processProcessedRips :: RipConfigExt -> ProcessedQueue -> IO ()
processProcessedRips config queue = go
  where
    go = do
      event <- atomically $ readTQueue queue
      case event of
        QValue processedEvent -> do
          putStrLn $ processedEventDescription processedEvent
          updateRSS config
          go

        QFinish -> pure ()

    processedEventDescription :: ProcessedEvent -> String
    processedEventDescription NewProcessedRip = "New processed rip"
    processedEventDescription InitialRSSUpdate = "Initial RSS update"

ensureDirs :: RipConfigExt -> IO ()
ensureDirs RipConfigExt{rawRipDir, cleanRipDir, trashRawRipDir, doneRipDir} = do
  let createParents = True
      ensureDir = createDirectoryIfMissing createParents
  forM_ [rawRipDir, cleanRipDir, trashRawRipDir, doneRipDir] ensureDir

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

processedRipSuffix :: IsString s => s
processedRipSuffix = "_enc"

processRip :: RipConfigExt -> Ripper.SuccessfulRip -> IO ()
processRip configExt@RipConfigExt{config, doneRipDir} newRip = do
  -- TODO get year from the file itself
  year <- show . fst . toOrdinalDate . localDay . zonedTimeToLocalTime <$> getZonedTime
  processRip' year newRip

  where
    processRip' :: String -> Ripper.SuccessfulRip -> IO ()
    processRip' year Ripper.SuccessfulRip{Ripper.ripFilename=ripName, Ripper.ripMP3Structure=mp3} = do
      podTitle <- podTitleFromFilename ripName
      let processedRip = processedRipNameFromOriginal doneRipDir ripName
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
        .| sinkFile processedRip
      trashFile configExt ripName

-- | Puts the given `file` into the rip's `trashRawRipDir`, cleaning up 15+ days
-- old files from it and `rawRipDir` beforehand.
trashFile :: RipConfigExt -> FilePath -> IO ()
trashFile RipConfigExt{trashRawRipDir, rawRipDir} file =
  clean trashRawRipDir *> clean rawRipDir *> trash

  where
    trash = do
      let targetFile = trashRawRipDir </> takeFileName file
      putStrLn $ mconcat ["Moving ", file, " to ", targetFile]
      renameFile file targetFile

    clean dir = do
      now <- getCurrentTime
      allFiles <- fmap (dir </>) <$> listDirectory dir
      let tooOld = (> 15 * nominalDay)
      oldFiles <- filterM (fmap (tooOld . diffUTCTime now) . getModificationTime) allFiles
      unless (null oldFiles) $ do
        putStrLn $ "Removing old trash files: " <> intercalate ", " oldFiles
        forM_ oldFiles removeFile

podTitleFromFilename :: FilePath -> IO String
podTitleFromFilename name = fromMaybe "" <$> readCommand
  -- FIXME replace with a native Haskell solution
  "sed"
  ["-nE", "s/.*([0-9]{4})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2}).*/\\1-\\2-\\3 \\4:\\5:\\6/p"]
  name

processedRipNameFromOriginal :: FilePath -> FilePath -> FilePath
processedRipNameFromOriginal doneRipDir ripName = doneRipDir </> takeBaseName ripName <> processedRipSuffix <.> "mp3"

-- | Calculates `MP3Structure` of the given `file` by parsing it.
mp3StructureFromFile :: FilePath -> IO MP3Structure
mp3StructureFromFile file = runConduitRes $
  sourceFile file
    -- FIXME this is very similar to, but not the same as, what happens while
    -- ripping; is it possible to reuse the code?
    .| conduitParserEither maybeFrameParser
    .| C.mapMaybeM getMP3Frame
    .| mapC dropFrameData
    .| foldlC extendMP3 (MP3Structure mempty)

  where
    getMP3Frame :: MonadIO m
      => Either ParseError (PositionRange, MaybeFrame) -> m (Maybe FullFrame)
    getMP3Frame (Right (_, Valid f)) = pure $ Just f
    getMP3Frame (Right (posRange, Junk l)) = do
      liftIO . putStrLn $ mconcat
        [ "Found junk in stream: "
        , show l, " bytes long at bytes "
        , show . posOffset . posRangeStart $ posRange
        , "-"
        , show . posOffset . posRangeEnd $ posRange
        ]
      pure Nothing
    getMP3Frame (Left e) = do
      liftIO . putStrLn $ "Parse error: " <> show e
      pure Nothing

    extendMP3 :: MP3Structure -> ShallowFrame -> MP3Structure
    extendMP3 mp3 frame = MP3Structure $ unMP3Structure mp3 `V.snoc` frame

{- |
 - discover and process original rips in the source dir, which may be
 - there as a result of a crash
 -}
processPreviousRips :: RipConfigExt -> ProcessedQueue -> IO ()
processPreviousRips configExt@RipConfigExt{config, doneRipDir, cleanRipDir} queue = do
  ripOriginals <- fmap (cleanRipDir </>) . filter isMP3 <$> listDirectory cleanRipDir
  ripOriginals' <- traverse
    (\ripName -> do
      mp3 <- mp3StructureFromFile ripName
      pure (Ripper.SuccessfulRip ripName mp3, processedRipNameFromOriginal doneRipDir ripName)
    )
    ripOriginals

  -- TODO get year from the file itself
  year <- show . fst . toOrdinalDate . localDay . zonedTimeToLocalTime <$> getZonedTime
  forM_ ripOriginals' (processRip' year)

  where
    isMP3 = (== ".mp3") . takeExtension

    -- FIXME almost a duplicate of the same-named function above
    processRip' :: String -> (Ripper.SuccessfulRip, FilePath) -> IO ()
    processRip' year (Ripper.SuccessfulRip{Ripper.ripFilename=ripName, Ripper.ripMP3Structure=mp3}, processedRip) = do
      podTitle <- podTitleFromFilename ripName
      let id3Header = getID3Header . generateID3Header $ ID3Fields
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
        .| sinkFile processedRip
      trashFile configExt ripName
      atomically $ writeTQueue queue $ QValue NewProcessedRip

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
