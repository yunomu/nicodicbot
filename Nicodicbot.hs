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
    return $ curlOK code Nothing $ either (\a -> Nothing) f $ art content
  where
    f a = Just a{a_title = rss_title entry,
                 a_date = rss_date entry}
    art content = getArticle $ decodeString content

curlOK :: CurlCode -> a -> a -> a
curlOK CurlOK _   a = a
curlOK _      def _ = def

main :: IO ()
main = do
    config <- loadConfig "nicodicbot.config"
    (code, rss) <- curlGetString (cfg_rssuri config) []
    curlOK code (fail "cannot get rss") $ return ()
    let es = entries $ decodeString rss
    let keys = cfg_keywords config
    articles <- P.mapM (f keys) es
    mapM_ print $ catMaybes articles
  where
    f :: [String] -> Entry -> IO (Maybe Article)
    f keys entry = do
        ma <- article entry
        return $ maybe Nothing (toMaybe $ strContain keys) ma

    toMaybe :: (a -> Bool) -> a -> Maybe a
    toMaybe b a = if b a then Just a else Nothing

