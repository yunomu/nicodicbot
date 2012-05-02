module RssParser (Entry(..), entries) where

import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Util
import Data.Time
import System.Locale

data Entry = Entry {
    rss_title :: String,
    rss_link :: String,
    rss_date :: Maybe ZonedTime}

instance Show Entry where
    show e = "{title = \"" ++ rss_title e ++
             "\", link = " ++ rss_link e ++
             ", date = " ++ show (rss_date e) ++ "}"

entries :: String -> [Entry]
entries = map entry . (deep $ tag "item") . rootContent

rootContent :: String -> Content Posn
rootContent str = cont
  where
    Document _ _ rootElement _ = xmlParse "" str
    cont = CElem rootElement noPos

entry :: Content Posn -> Entry
entry item = Entry {
    rss_title = tagText "title" item,
    rss_link = tagText "link" item,
    rss_date = date $ tagText "dc:date" item}
  where
    date str = parseTime defaultTimeLocale fmt str
    fmt = "%FT%X%z"

tagText :: String -> Content Posn -> String
tagText t = tagTextContent . head . (deep $ tag t)

