module Podripper
  ( RipName
  , run
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.Aeson hiding ((<?>))
import qualified Data.Aeson.KeyMap as A
import Data.Functor
import Data.Maybe
import Data.List (isSuffixOf)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TL
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Network.HTTP.Simple
import RIO (whenM, IsString, threadDelay)
import qualified RSSGen.Main as RSSGen (run)
import RipConfig
import qualified Ripper.Main as Ripper (run)
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

run :: RipName -> IO ()
run ripName = do
  skipRipping <- getSkipRipping
  config <- loadConfig ripName
  let configExt = extendConfig config
  ensureDirs configExt
  -- | the flag shows whether the live stream check has returned success since
  -- the start; we don't need to ask it anymore after that
  streamIsLive <- waitForStream configExt
  when (streamIsLive && not skipRipping) $ rip configExt
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

waitForStream :: RipConfigExt -> IO Bool
waitForStream RipConfigExt{config} =
  let ripName = ripDirName config
  -- the `atp` support is hardcoded in the program because its live stream check
  -- is more complicated and the stream URL needs to be extracted from the
  -- status endpoint
  -- FIXME support this via the config file
  in if ripName == "atp" then
    toProcessReady <$> runFor
      (retrySec config)
      (fromIntegral $ durationSec config)
      (fromProcessReady <$> waitForATP)
    else pure True

  where
    waitForATP = logError <=< runExceptT $ do
      statusResponse <- liftIO . fmap getResponseBody . httpLBS $ parseRequest_ "https://atp.fm/livestream_status"
      liftIO . TL.putStrLn $ TLE.decodeUtf8 statusResponse
      status <- liftEither . eitherDecode @Object $ statusResponse

      isLiveValue <- liftEither $ A.lookup "live" status <?> "Can't find `live` key"
      liftEither $ extractBool isLiveValue

    extractBool :: Value -> Either String Bool
    extractBool (Bool b) = Right b
    extractBool x = Left $ "Expected a bool, got " <> show x

    logError :: Either String Bool -> IO Bool
    logError (Right b) = pure b
    logError (Left err) = putStrLn err $> False

rip :: RipConfigExt -> IO ()
rip RipConfigExt{config, rawRipDir} =
  let options = Ripper.Options
        { Ripper.optionsVerbose = True
        , Ripper.optionsOutputDirectory = Just rawRipDir
        , Ripper.optionsRipLengthSeconds = fromIntegral $ durationSec config
        , Ripper.optionsReconnectDelay = fromIntegral $ retrySec config
        , Ripper.optionsSmallReconnectDelay = 1
        , Ripper.optionsStreamURL = streamURL config
        }
  -- note: this loop is not needed on its own because the ripper should already
  -- run for `durationSec`; however, this is a guard to restart it in case it
  -- throws an exception, so `catchExceptions` is required
  in runFor (retrySec config) (fromIntegral $ durationSec config) $ do
    putStrLn "starting the ripper"
    catchExceptions $ Ripper.run options

data ProcessReadiness = NotReady | Ready
  deriving (Eq, Show)

fromProcessReady :: Bool -> ProcessReadiness
fromProcessReady True = Ready
fromProcessReady False = NotReady

toProcessReady :: ProcessReadiness -> Bool
toProcessReady Ready = True
toProcessReady NotReady = False

-- | Defines the types that can represent process readiness flag, basically
-- `ProcessReadiness`, or `()` for cases when process readiness is meaningless.
-- The whole point of this typeclass is `showReadiness`, which returns `Nothing`
-- for `()` so that it's not logged! There is also a (runtime) validation that
-- the ready case doesn't make sense for `()` (I'm not sure how to make it a
-- compile-time check; `absurd` doesn't work). This is probably an overkill for
-- such a small use case and should be removed when/if process readiness logging
-- is removed, and there is no observable difference between the two instances.
class ProcessReadinessType a where
  mkReady :: a
  mkNotReady :: a
  readinessIsReady :: a -> Bool
  showReadiness :: a -> Maybe String

instance ProcessReadinessType ProcessReadiness where
  mkReady = Ready
  mkNotReady = NotReady
  readinessIsReady = toProcessReady
  showReadiness = Just . show

instance ProcessReadinessType () where
  mkReady = error "impossible case: mkReady for ()"
  mkNotReady = ()
  readinessIsReady = const False
  showReadiness = const Nothing

-- | Runs the given IO action repeatedly for the provided `duration` until it
-- returns the `Ready` state, with the `retryDelaySec` between each invocation.
-- If it isn't `Ready` until the `duration` expires, the final state is what
-- the action returns (that is, `NotReady`).
runFor :: ProcessReadinessType r => Int -> NominalDiffTime -> IO r -> IO r
runFor retryDelaySec duration io = do
  now <- getCurrentTime
  let endTime = addUTCTime duration now
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
        let nextNow = addUTCTime (fromIntegral retryDelaySec) afterIO
        let haveEnoughTimeForNextIteration = nextNow < endTime
        putStrLn . mconcat $
          [ "now ", show afterIO
          , "; have enough time for next iteration: ", show haveEnoughTimeForNextIteration
          ]
          <> maybe [] (\s -> ["; process readiness: ", s]) (showReadiness processReadiness)

        -- TODO how to split these two different concerns: wait until time and
        -- wait until ready?
        if readinessIsReady processReadiness then pure mkReady
        else if haveEnoughTimeForNextIteration then do
          threadDelay $ retryDelaySec * microsecondsInSecond
          go endTime
        -- TODO is it possible to simplify the implementation? there are too
        -- many return points here
        else pure mkNotReady

      -- didn't manage to get the ready status before out-of-time => not ready
      else pure mkNotReady

    microsecondsInSecond = 1_000_000

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
  withCurrentDirectory doneBaseDir $ RSSGen.run rssName
  -- FIXME replace `ripDirName` with the requested rip name and remove the field
  where rssName = NE.singleton $ T.unpack (ripDirName config) <.> "rss"

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

getConfDir :: IO FilePath
getConfDir = do
  maybeConfDir <- lookupEnv "CONF_DIR"
  pure $ fromMaybe "/usr/share/podripper" maybeConfDir

-- infix 7 <?>
(<?>) :: Maybe a -> b -> Either b a
Just x <?> _ = Right x
Nothing <?> e = Left e
