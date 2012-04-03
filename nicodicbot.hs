import System.IO
import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Util
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Network.Curl hiding (Content)

import Config

data Entry = Entry {
    title :: String,
    link :: String}

instance Show Entry where
    show e = "{title = \"" ++ title e ++ "\", link = " ++ link e ++ "}"

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
        title = tagText "title" item,
        link = tagText "link" item}

    tagText :: String -> Content Posn -> String
    tagText t = tagTextContent . head . (deep $ tag t)

main :: IO ()
main = do
    config <- loadConfig "nicodicbot.config"
    (code, rss) <- curlGetString (rssUri config) []
    mapM_ (putStrLn . show) $ entries $ decodeString rss

