module Main where

import System.IO
import Codec.Binary.UTF8.String (decodeString)
import Network.Curl
import Data.Maybe
import qualified Control.Monad.Parallel as P

import Config
import RssParser
import Article

article :: Entry -> IO (Maybe Article)
article entry = do
    (code, contents) <- curlGetString (rss_link entry) []
    return $ curlOK code Nothing $ either (\_ -> Nothing) f $ art contents
  where
    f a = Just a{a_title = rss_title entry,
                 a_date = rss_date entry}
    art contents = getArticle $ decodeString contents

curlOK :: CurlCode -> a -> a -> a
curlOK CurlOK _   a = a
curlOK _      def _ = def

main :: IO ()
main = do
    hSetEncoding stdout utf8
    config <- loadConfig "nicodicbot.config"
    (code, rss) <- curlGetString (cfg_rssuri config) []
    curlOK code (fail "Couldn't get rss") $ return ()
    let es = entries $ decodeString rss
    let keys = cfg_keywords config
    articles <- P.mapM (f keys) es
--    mapM_ print $ catMaybes articles
    mapM_ (putStrLn . dump) $ catMaybes articles
  where
    f :: [String] -> Entry -> IO (Maybe Article)
    f keys entry = do
        ma <- article entry
        return $ maybe Nothing (toMaybe $ strContain keys) ma

    toMaybe :: (a -> Bool) -> a -> Maybe a
    toMaybe b a = if b a then Just a else Nothing

