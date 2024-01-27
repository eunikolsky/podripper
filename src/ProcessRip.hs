module ProcessRip
  ( processRip'
  ) where

import Conduit
import Control.Monad
import Data.Conduit.List qualified as C
import Data.List (intercalate)
import Data.Maybe
import Data.Text qualified as T
import Data.Time
import MP3.ID3
import MP3.Xing
import RipConfig
import Ripper.Types qualified as Ripper
import Ripper.Util
import System.Directory
import System.FilePath

processRip' :: RipConfigExt -> String -> (Ripper.SuccessfulRip, FilePath) -> IO ()
processRip'
    configExt@RipConfigExt{config}
    year
    (Ripper.SuccessfulRip{Ripper.ripFilename=ripName, Ripper.ripMP3Structure=mp3}, processedRip)
  = do
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

podTitleFromFilename :: FilePath -> IO String
podTitleFromFilename name = fromMaybe "" <$> readCommand
  -- FIXME replace with a native Haskell solution
  "sed"
  ["-nE", "s/.*([0-9]{4})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2}).*/\\1-\\2-\\3 \\4:\\5:\\6/p"]
  name

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
