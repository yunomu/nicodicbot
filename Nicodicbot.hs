module Nicodicbot where

import System.IO
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Network.Curl hiding (Content)

import Config
import RssParser
import ArticleParser

article :: Entry -> IO (Maybe Article)
article entry = do
    (CurlOK, content) <- curlGetString (link entry) []
    case getArticle (title entry) (decodeString content) of
--      Left msg -> ioError $ userError $ (title entry) ++ ": " ++ msg
      Left mst -> return Nothing
      Right a  -> return $ Just a

main :: IO ()
main = do
    config <- loadConfig "nicodicbot.config"
    (CurlOK, rss) <- curlGetString (rssUri config) []
    articles <- mapM article $ entries $ decodeString rss
    mapM_ (putStrLn . show) articles

