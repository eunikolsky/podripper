module RSSFeed where

import Text.XML.Light

-- | Returns the XML string with the entire RSS feed.
feed :: String
feed = ppElement rss
  where
    rss = unode "rss" ([Attr (unqual "version") "2.0", xmlns_atom], channel)
    atomns_name = "atom"
    xmlns_atom = Attr (QName atomns_name Nothing (Just "xmlns")) "http://www.w3.org/2005/Atom"
    channel = unode "channel" [link, title, language, description, generator, image, atom_link]

    link = unode "link" "https://example.com/"
    title = unode "title" "Поток"
    language = unode "language" "ru"
    description = unode "description" "foo"
    generator = unode "generator" "rssgen"
    image = unode "image" $ unode "url" "https://example.com/cover.jpg"
    atom_link = node (QName "link" Nothing (Just atomns_name))
      [ Attr (unqual "rel") "self"
      , Attr (unqual "type") "application/rss+xml"
      , Attr (unqual "href") "https://example.com/sample.rss"
      ]
