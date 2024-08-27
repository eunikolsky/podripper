module ProcessRip
  ( mp3StructureFromFile
  , processRip'
  ) where

import Conduit
import Control.Monad
import Data.Conduit.Attoparsec
import Data.Conduit.List qualified as C
import Data.List (intercalate)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Version (showVersion)
import MP3.ID3
import MP3.Parser
import MP3.Xing
import Paths_ripper qualified
import Rip
import RipConfig
import Ripper.Types qualified as Ripper
import System.Directory
import System.FilePath hiding (isValid)

-- | Processes a successful rip (using its parsed MP3 structure, that is, only
-- the full and correct MP3 frames) by prepending an ID3 header and Xing header.
-- Returns `True` if the rip is valid, that is, having 1+ MP3 frames.
processRip' :: RipConfigExt -> (Ripper.SuccessfulRip, FilePath) -> IO Bool
processRip'
    configExt@RipConfigExt{config}
    (Ripper.SuccessfulRip{Ripper.ripFilename=ripName, Ripper.ripMP3Structure=mp3, Ripper.ripStreamURL=streamURL}, processedRip)
  = do
    let isValid = isValidMP3 mp3
    when isValid $ doProcessRip
    trashFile configExt ripName
    pure isValid

  where
    doProcessRip = do
      let ripDate = fromMaybe (error $ "Couldn't parse rip time from filename " <> ripName) $ parseRipDate ripName
      ripTime <- localTimeToZonedTime ripDate
      now <- getCurrentTime
      let id3Header = getID3Header . generateID3Header $ ID3Fields
            { id3Title = T.pack . titlePubDate $ fst ripTime
            , id3Artist = podArtist config
            , id3Album = podAlbum config
            , id3RecordingTime = snd ripTime
            , id3Genre = "Podcast"
            , id3Publisher = "podripper/" <> version
            , id3Duration = audioDuration
            , id3EncodingTime = now
            , id3Language = podLanguage config
            , id3MediaType = "Internet stream"
            , id3PodcastURL = mkID3URL $ podHomepage config
            , id3StreamURL = mkID3URL . Ripper.urlToText . Ripper.getStreamURL <$> streamURL
            , id3OriginalFilename = T.pack $ takeFileName ripName
            }
          (XingHeader xingHeader, audioDuration) = calculateXingHeader mp3

      runConduitRes $
        C.sourceList [id3Header, xingHeader] *> sourceFile ripName
        .| sinkFile processedRip

-- FIXME version information is retrieved in multiple places
version :: Text
version = T.pack $ showVersion Paths_ripper.version

-- | Calculates `MP3Structure` of the given `file` by parsing it.
mp3StructureFromFile :: FilePath -> IO MP3Structure
mp3StructureFromFile file = fmap MP3Structure . runConduitRes $
  sourceFile file
    -- FIXME this is very similar to, but not the same as, what happens while
    -- ripping; is it possible to reuse the code?
    .| conduitParserEither maybeFrameParser
    .| C.mapMaybeM getMP3Frame
    .| mapC fInfo
    .| sinkVector

  where
    getMP3Frame :: MonadIO m
      => Either ParseError (PositionRange, MaybeFrame) -> m (Maybe Frame)
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

-- | Puts the given `file` into the rip's `trashRawRipDir`, cleaning up old
-- files from it and `rawRipDir` beforehand. Whether a file is old, is
-- determined by the number of days `oldFileDays` in `RipConfig`.
trashFile :: RipConfigExt -> FilePath -> IO ()
trashFile RipConfigExt{trashRawRipDir, rawRipDir, config=RipConfig{oldFileDays}} file =
  clean trashRawRipDir *> clean rawRipDir *> trash

  where
    trash = do
      let targetFile = trashRawRipDir </> takeFileName file
      putStrLn $ mconcat ["Moving ", file, " to ", targetFile]
      renameFile file targetFile

    clean dir = do
      now <- getCurrentTime
      allFiles <- fmap (dir </>) <$> listDirectory dir
      let tooOld = (> fromIntegral oldFileDays * nominalDay)
      oldFiles <- filterM (fmap (tooOld . diffUTCTime now) . getModificationTime) allFiles
      unless (null oldFiles) $ do
        putStrLn $ "Removing old trash files: " <> intercalate ", " oldFiles
        forM_ oldFiles removeFile
