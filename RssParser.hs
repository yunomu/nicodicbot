module RssParser where

import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Util

data Entry = Entry {
    rss_title :: String,
    rss_link :: String}

instance Show Entry where
    show e = "{title = \"" ++ rss_title e ++
             "\", link = " ++ rss_link e ++ "}"

entries :: String -> [Entry]
entries = map entry . (deep $ tag "item") . rootContent
  where
    rootContent :: String -> Content Posn
    rootContent str = cont
      where
        Document _ _ rootElement _ = xmlParse "" str
        cont = CElem rootElement noPos

    entry :: Content Posn -> Entry
    entry item = Entry {
        rss_title = tagText "title" item,
        rss_link = tagText "link" item}

    tagText :: String -> Content Posn -> String
    tagText t = tagTextContent . head . (deep $ tag t)

