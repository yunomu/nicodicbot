import System.IO
import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Util
import Codec.Binary.UTF8.String (encodeString, decodeString)

import Network.Curl hiding (Content)
{- test stub
curlGetString :: String -> [a] -> IO (a, String)
curlGetString path _ = do
    str <- readFile "test/new.rss"
    return (200, str)
---}

data Config = Config { rssUri :: String }
  deriving (Show)

data Entry = Entry { title :: String }
  deriving (Show)

getConfig :: IO Config
getConfig = return Config {rssUri = "http://dic.nicovideo.jp/feed/rss/u/a"}

entries :: String -> [Entry]
entries = map entry . (deep $ tag "title") . rootContent
  where
    rootContent :: String -> Content Posn
    rootContent str = cont
      where
        Document _ _ rootElement _ = xmlParse "" str
        cont = CElem rootElement noPos

    entry :: Content Posn -> Entry
    entry t = Entry {title = tagTextContent t}

main :: IO ()
main = do
    config <- getConfig
    (code, rss) <- curlGetString (rssUri config) []
    mapM_ (putStrLn . title) $ entries $ decodeString rss

