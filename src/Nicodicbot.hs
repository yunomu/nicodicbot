{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Main where

import System.IO
import System.Environment
import System.Exit
import Data.ByteString
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Network.HTTP.Conduit
import Data.Functor

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Concurrent

import Config
--import Article
import RssParser

curl :: (MonadResource m, MonadBaseControl IO m) =>
    String -> m (ResumableSource m ByteString)
curl url = do
    request <- liftIO $ parseUrl url
    manager <- liftIO $ newManager def
    responseBody <$> http request manager

procArticle :: Item -> IO ()
procArticle item = runResourceT $ do
    body <- (curl $ BC.unpack $ link item) >>= ($$+- CB.take 10240)
    liftIO $ print item

procEntries :: (MonadResource m, MonadBaseControl IO m) =>
    GLSink ByteString m ()
procEntries = do
    item <- itemParser
--    liftIO $ forkIO $ procArticle item
    liftIO $ procArticle item
    procEntries

main :: IO ()
main = do
    config <- getConfig
    let uri = rssuri config
    runResourceT $ do
        curl uri >>= ($$+- procEntries)
{-
    let rss = decodeString $ LC.unpack contents
    let keywords = cfg_keywords config
    articles <- fmap catMaybes $ P.mapM (f keywords) $ entries rss
--    store S.def articles
    mapM_ putStrLn $ show articles
  where
    f :: [String] -> (Entry -> IO (Maybe Article))
    f keys = \entry -> maybe
        Nothing
        (toMaybe $ strContain keys)
        <$> article entry

    toMaybe :: (a -> Bool) -> (a -> Maybe a)
    toMaybe g = \a -> if g a then Just a else Nothing

    article :: Entry -> IO (Maybe Article)
    article entry = do
        catch (do 
            contents <- curl $ rss_link entry
            let c = art contents
            print c -- debug
            return $ either fail ret c)
          (\_ -> do
            print entry
            return Nothing)
      where
        ret a = Just a{ a_title = rss_title entry
                      , a_date = rss_date entry
                      }
        art contents = getArticle $ decodeString $ LC.unpack contents
-}
  where
    getConfig :: IO Config
    getConfig = getArgs >>= eachCase
    eachCase args
      | n == 1    = loadConfig $ args !! 0
      | otherwise = do
          BC.hPutStrLn stderr "Usage: nicodicbot config_file"
          exitFailure
      where
        n = Prelude.length args

