{-# LANGUAGE OverloadedStrings #-}

module RSSFeed where

import Data.Aeson
import qualified Data.Text as T
import Text.XML.Light

import RSSItem
import Types

-- | Values for the RSS feed tags.
data RSSFeedConfig = RSSFeedConfig
  { title :: T.Text
  , description :: T.Text
  , language :: T.Text
  , podcastLink :: T.Text
  , imageLink :: T.Text
  , selfLink :: T.Text
  , upstreamRSSURL :: Maybe T.Text
  -- TODO extract this with `upstreamRSSURL` into a separate type because this
  -- interval doesn't make sense if there is no upstream URL?
  , closestUpstreamItemInterval :: Hours
  }
  deriving (Eq, Show)

instance FromJSON RSSFeedConfig where
  parseJSON = withObject "RSSFeedConfig" $ \v -> RSSFeedConfig
    <$> v .: "title"
    <*> v .: "description"
    <*> v .: "language"
    <*> v .: "podcastLink"
    <*> v .: "imageLink"
    <*> v .: "selfLink"
    <*> v .:? "upstreamRSSURL"
    -- note: the JSON key is slightly more specific because there is no type information there
    -- TODO is it possible to override the key in `FromJSON Hours` itself?
    <*> v .: "closestUpstreamItemIntervalHours"

-- | The program version to use in the RSS feed.
newtype ProgramVersion = ProgramVersion String

-- | Returns the XML string of the entire RSS feed with the RSS items.
feed :: ProgramVersion -> RSSFeedConfig -> [RSSItem] -> String
feed
    (ProgramVersion version)
    (RSSFeedConfig fcTitle fcDescription fcLanguage fcPodcastLink fcImageLink fcSelfLink _ _)
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
