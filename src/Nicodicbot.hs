module Main where

import System.IO
import System.Environment
import System.Exit
import Codec.Binary.UTF8.String (decodeString)
import Network.Curl
import Data.Maybe
import Data.Functor
import qualified Control.Monad.Parallel as P

import Config
import RssParser
import Article
import Store hiding (def)
import qualified Store as S

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
    config <- getConfig
    let uri = cfg_rssuri config
    (code, contents) <- curlGetString uri []
    curlOK code (fail $ "Couldn't get rss: " ++ uri) $ return ()
    let rss = decodeString contents
    let keywords = cfg_keywords config
    articles <- fmap catMaybes $ P.mapM (f keywords) $ entries rss
    store S.def articles
    mapM_ (putStrLn . dump) articles
  where
    f :: [String] -> (Entry -> IO (Maybe Article))
    f keys = \entry -> maybe
        Nothing
        (toMaybe $ strContain keys)
        <$> article entry

    toMaybe :: (a -> Bool) -> (a -> Maybe a)
    toMaybe g = \a -> if g a then Just a else Nothing

    getConfig :: IO Config
    getConfig = getArgs >>= eachCase
    eachCase args
      | n == 1    = loadConfig $ args !! 0
      | otherwise = do
          hPutStrLn stderr "Usage: nicodicbot config_file"
          exitFailure
      where
        n = length args

