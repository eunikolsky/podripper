{-# LANGUAGE OverloadedStrings #-}

module RSSGen.RSSFeed
  ( ProgramVersion(..)
  , RSSFeedConfig(..)
  , UpstreamFeedConfig(..)
  , feed
  , parseFeedConfig
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import qualified Data.Text as T
import Development.Shake.Classes
import GHC.Generics
import System.Directory
import System.FilePath
import Text.Show.Unicode
import Text.XML.Light

import RSSGen.Duration
import RSSGen.RSSItem (RSSItem, renderItem)

-- | Defines the config for the upstream RSS feed. This section may be missing
-- from the json config if there is no upstream feed.
data UpstreamFeedConfig = UpstreamFeedConfig
  { upstreamRSSURL :: T.Text
  , closestUpstreamItemInterval :: Duration
  -- | The max number of the most recent RSS entries to import, if set;
  -- otherwise, use all entries.
  , maxItems :: Maybe Int
  , pollingDuration :: Duration
  , pollingRetryDelay :: RetryDelay
  }
  deriving (Eq, Show, Generic, Hashable, Binary, NFData)

instance FromJSON UpstreamFeedConfig

-- | Values for the RSS feed tags.
data RSSFeedConfig = RSSFeedConfig
  { title :: T.Text
  , description :: T.Text
  , language :: T.Text
  , podcastLink :: T.Text
  , imageLink :: T.Text
  , selfLink :: T.Text
  , upstreamFeedConfig :: !(Maybe UpstreamFeedConfig)
  }
  deriving (Eq, Generic, Hashable, Binary, NFData)

instance Show RSSFeedConfig where
  show RSSFeedConfig{title,description,language,podcastLink,imageLink,selfLink,upstreamFeedConfig} = mconcat
    [ "RSSFeedConfig{"
    , "title = ", ushow title
    , ", description = ", ushow description
    , ", language = ", ushow language
    , ", podcastLink = ", ushow podcastLink
    , ", imageLink = ", ushow imageLink
    , ", selfLink = ", ushow selfLink
    , ", upstreamFeedConfig = ", ushow upstreamFeedConfig
    , "}"
    ]

instance FromJSON RSSFeedConfig

-- | The program version to use in the RSS feed.
newtype ProgramVersion = ProgramVersion String

-- | Returns the XML string of the entire RSS feed with the RSS items.
feed :: ProgramVersion -> RSSFeedConfig -> [RSSItem] -> String
feed
    (ProgramVersion version)
    (RSSFeedConfig fcTitle fcDescription fcLanguage fcPodcastLink fcImageLink fcSelfLink _)
    rssItems =
  ppcElement config rss
  where
    config = useShortEmptyTags (/= unqual "description") prettyConfigPP

    rss = unode "rss" ([Attr (unqual "version") "2.0", xmlns_atom], channel)
    atomns_name = "atom"
    xmlns_atom = Attr (QName atomns_name Nothing (Just "xmlns")) "http://www.w3.org/2005/Atom"
    channel = unode "channel" $ [link, title, language, description, generator, image, atom_link] <> items

    link = unodet "link" fcPodcastLink
    title = unodet "title" fcTitle
    language = unodet "language" fcLanguage
    description = unodet "description" fcDescription
    generator = unode "generator" $ "rssgen " <> version
    image = unode "image" $ unodet "url" fcImageLink
    atom_link = node (QName "link" Nothing (Just atomns_name))
      [ Attr (unqual "rel") "self"
      , Attr (unqual "type") "application/rss+xml"
      , Attr (unqual "href") $ T.unpack fcSelfLink
      ]

    items = renderItem (T.unpack fcPodcastLink) <$> rssItems

    unodet name = unode name . T.unpack

type PodcastTitle = String

-- | Parses `RSSFeedConfig` for podcast with `podcastTitle` from one or two
-- config files in the directory `dir`, returns the result and the used filenames
-- (this is necessary for Shake to keep track of file changes). To parse the
-- config successfully:
-- * the base config file must exist and have a valid json object (could be empty);
-- * if the overlay file exists, it must contain a valid json object, otherwise
--   it's ignored; the fields are added or overwrite the fields in the base object;
-- * the resulting object must be parseable into `RSSFeedConfig`.
parseFeedConfig :: FilePath -> PodcastTitle -> IO (Maybe RSSFeedConfig, [FilePath])
parseFeedConfig dir podcastTitle = do
  overlayFileExists <- doesFileExist overlayFilename
  maybeConfig <- runMaybeT $ do
    baseFileExists <- liftIO $ doesFileExist filename
    guard baseFileExists
    baseConfigValue <- MaybeT $ decodeFileStrict' filename
    let decodeOverlayFile = MaybeT $ decodeFileStrict' overlayFilename
        emptyOverlayFile = pure . Object $ KM.empty
    overlayConfigValue <- if overlayFileExists then decodeOverlayFile else emptyOverlayFile
    let configValue = mergeJSONValues baseConfigValue overlayConfigValue
    MaybeT . pure $ parseMaybe parseJSON configValue

  let filenames = (if overlayFileExists then (overlayFilename :) else id) [filename]
  pure (maybeConfig, filenames)

  where
    filename = dir </> podcastTitle <> "_feed" <.> "json"
    overlayFilename = dir </> podcastTitle <> "_feed_overlay" <.> "json"

mergeJSONValues :: Value -> Value -> Value
mergeJSONValues (Object base) (Object overlay) = Object $ KM.union overlay base
mergeJSONValues _ _ = error "mergeJSONValues expects to merge only objects"
