module Main where

import System.IO
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Network.Curl hiding (Content)
import Data.Maybe
import Data.Time
import qualified Control.Monad.Parallel as P

import Config
import RssParser
import ArticleParser

article :: Entry -> IO (Maybe Article)
article entry = do
    (code, content) <- curlGetString (rss_link entry) []
    return $ curlProc code $ either (\a -> Nothing) f $ art content
  where
    f a = Just a{a_title = rss_title entry,
                 a_date = rss_date entry}
    art content = getArticle $ decodeString content

    curlProc CurlOK a = a
    curlProc _      _ = Nothing

main :: IO ()
main = do
    config <- loadConfig "nicodicbot.config"
    (CurlOK, rss) <- curlGetString (cfg_rssuri config) []
    let es = entries $ decodeString rss
    articles <- P.mapM article es
    let as = catMaybes articles
    let keys = cfg_keyword config
    P.mapM print $ filter (strContain keys) as
    return ()

