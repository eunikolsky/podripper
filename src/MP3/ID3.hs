module MP3.ID3
  ( ID3Fields(..)
  , ID3Header(..)
  , generateID3Header
  , mkID3URL
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Builder qualified as BSB
import Data.Char
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.Word (Word32)
import MP3.AudioDuration

-- | A URL that can be used in an ID3 tag. The reason it's a bytestring and not
-- `Text` is the ID3 v2.4 spec says URLs should be in ISO-8859-1 (aka Latin-1),
-- and `ByteString` represents that better than a unicode `Text`.
newtype ID3URL = ID3URL { getID3URL :: ByteString }

-- | Converts the text to a Latin-1 ID3 URL. All unsupported characters are
-- replaced with a question mark.
mkID3URL :: Text -> ID3URL
mkID3URL = ID3URL . C.pack . T.unpack . T.map replaceUnsupported
  where replaceUnsupported c = if c >= chr 0x20 && c <= chr 0xff then c else '?'

data ID3Fields = ID3Fields
  { id3Title :: !Text
  , id3Artist :: !Text
  , id3Album :: !Text
  , id3RecordingTime :: !UTCTime
  , id3Genre :: !Text
  , id3Publisher :: !Text
  , id3Duration :: !AudioDuration
  , id3EncodingTime :: !UTCTime
  , id3Language :: !Text
  -- ^ A three-letter ISO-639-2 language code
  , id3MediaType :: !Text
  , id3PodcastURL :: !ID3URL
  , id3StreamURL :: !(Maybe ID3URL)
  -- ^ Podcasts with a live check (ATP) may not have a static stream URL (even
  -- though one is written in the rip config), thus podcasts that are processed
  -- after program start (if it crashed) won't have this information.
  , id3OriginalFilename :: !Text
  }

newtype ID3Header = ID3Header { getID3Header :: ByteString }

-- | Generates an ID3 v2.4 header with the given fields.
generateID3Header :: ID3Fields -> ID3Header
generateID3Header ID3Fields
    { id3Title, id3Artist, id3Album, id3RecordingTime, id3Genre, id3Publisher
    , id3Duration, id3EncodingTime, id3Language, id3MediaType, id3PodcastURL
    , id3StreamURL, id3OriginalFilename
    } =
  ID3Header . BS.toStrict . BSB.toLazyByteString $ header <> BSB.lazyByteString frames

  where
    header = mconcat [id3Id, version, flags, size]
    id3Id = "ID3"
    version = "\x04\x00"
    flags = BSB.word8 0
    size = BSB.word32BE . syncSafe . fromIntegral . BSL.length $ frames

    frames = BSB.toLazyByteString $ mconcat
      [ textFrame frameTitle id3Title
      , textFrame frameLeadPerformer id3Artist
      , textFrame frameAlbum id3Album
      , textFrame frameRecordingTime $ formatID3Time id3RecordingTime
      , textFrame frameContentType id3Genre
      , textFrame framePublisher id3Publisher
      , textFrame frameLength $ inMilliseconds id3Duration
      , textFrame frameEncodingTime $ formatID3Time id3EncodingTime
      , textFrame frameLanguage id3Language
      , textFrame frameMediaType id3MediaType
      , urlFrame frameOfficialInternetRadioStationHomepage id3PodcastURL
      , maybe mempty (urlFrame frameOfficialAudioSourceWebpage) id3StreamURL
      , textFrame frameOriginalFilename id3OriginalFilename
      ]

frameTitle, frameLeadPerformer, frameAlbum, frameRecordingTime
  , frameContentType, framePublisher, frameLength, frameEncodingTime
  , frameLanguage, frameMediaType, frameOfficialInternetRadioStationHomepage
  , frameOfficialAudioSourceWebpage, frameOriginalFilename :: ByteString
frameTitle = "TIT2"
frameLeadPerformer = "TPE1"
frameAlbum = "TALB"
frameRecordingTime = "TDRC"
frameContentType = "TCON"
framePublisher = "TPUB"
frameLength = "TLEN"
frameEncodingTime = "TDEN"
frameLanguage = "TLAN"
frameMediaType = "TMED"
frameOfficialInternetRadioStationHomepage = "WORS"
frameOfficialAudioSourceWebpage = "WOAS"
frameOriginalFilename = "TOFN"

formatID3Time :: UTCTime -> Text
formatID3Time = T.pack . formatTime defaultTimeLocale "%FT%T"

inMilliseconds :: AudioDuration -> Text
inMilliseconds = T.pack . show @Int . floor . (* 1000) . getAudioDuration

frame :: ByteString -> ByteString -> BSB.Builder
frame frameId contents =
  mconcat [BSB.byteString frameId, size, flags, BSB.byteString contents]

  where
    size = BSB.word32BE . syncSafe . fromIntegral . BS.length $ contents
    flags = BSB.word16BE 0

textFrame
  :: ByteString
  -- ^ Frame ID, must be 4 characters
  -> Text
  -- ^ Frame value
  -> BSB.Builder
textFrame frameId value = frame frameId $ mconcat [utf8Marker, utf8Value, utf8Terminator]
  where
    utf8Marker = "\x03"
    utf8Value = TE.encodeUtf8 value
    utf8Terminator = "\x00"

urlFrame :: ByteString -> ID3URL -> BSB.Builder
urlFrame frameId = frame frameId . getID3URL

-- | Returns a "synchsafe integer" for the given integer: the most-significant
-- bit of every byte is reset, thus it can store 28 bits of information.
syncSafe :: Word32 -> Word32
syncSafe x =
  (x .&. 0x7f)
  .|. ((x `shiftL` 1) .&. 0x7f00)
  .|. ((x `shiftL` 2) .&. 0x7f0000)
  .|. ((x `shiftL` 3) .&. 0x7f000000)
