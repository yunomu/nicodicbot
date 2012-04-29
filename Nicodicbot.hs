module Main where

import System.IO
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Network.Curl hiding (Content)
import Data.Maybe

import Config
import RssParser
import ArticleParser

article :: Entry -> IO (Maybe Article)
article entry = do
    (CurlOK, content) <- curlGetString (RssParser.link entry) []
    case getArticle (RssParser.title entry) (decodeString content) of
--      Left msg -> ioError $ userError $ (title entry) ++ ": " ++ msg
      Left msg -> return Nothing
      Right a  -> return $ Just a

main :: IO ()
main = do
    config <- loadConfig "nicodicbot.config"
    (CurlOK, rss) <- curlGetString (rssUri config) []
    let es = entries $ decodeString rss
    articles <- mapM article $ es
    let as = catMaybes articles
    let keys = keyWord config
--    mapM_ print $ filter (strContain keys) as
    mapM_ print as
