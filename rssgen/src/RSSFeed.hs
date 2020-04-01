{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module RSSFeed where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Text.XML.Light

import RSSItem

-- | Values for the RSS feed tags.
data RSSFeedConfig = RSSFeedConfig
  { title :: T.Text
  , description :: T.Text
  , language :: T.Text
  , podcastLink :: T.Text
  , imageLink :: T.Text
  , selfLink :: T.Text
  }
  deriving (Generic, FromJSON, Show, ToJSON)

-- | The program version to use in the RSS feed.
newtype ProgramVersion = ProgramVersion String

-- | Returns the XML string of the entire RSS feed with the RSS items.
feed :: ProgramVersion -> RSSFeedConfig -> [RSSItem] -> String
feed
    (ProgramVersion version)
    (RSSFeedConfig fcTitle fcDescription fcLanguage fcPodcastLink fcImageLink fcSelfLink)
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
