{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where

import System.IO
import System.Environment
import System.Exit
import Data.ByteString
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import Network.HTTP.Conduit
import Data.Functor

import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import Config
import RssParser

curl :: (MonadResource m, MonadBaseControl IO m) =>
    String -> m (ResumableSource m ByteString)
curl url = do
    request <- liftIO $ parseUrl url
    manager <- liftIO $ newManager def
    responseBody <$> http request manager

main :: IO ()
main = do
    config <- getConfig
    let uri = cfg_rssuri config
    runResourceT $ do
        rss <- curl uri
        (src1, entry1) <- rss $$++ itemParser
        liftIO $ print entry1
        return ()
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

