module Main where

import System.IO
import System.Environment
import System.Exit

import Config
import Store
import Twitter
import Article

main :: IO ()
main = do
    config <- getConfig
    articles <- unpostedArticles def
    let keys = twKeys
          (cfg_consumer_key config)
          (cfg_consumer_secret config)
          (cfg_access_token config)
          (cfg_access_token_secret config)
--    mapM_ (exec keys) articles
    exec keys b
  where
    getConfig :: IO Config
    getConfig = getArgs >>= eachCase
    eachCase args
      | n == 1    = loadConfig $ args !! 0
      | otherwise = do
          hPutStrLn stderr "Usage: ndpost config_file"
          exitFailure
      where
        n = length args

    exec :: TwKeys -> Article -> IO ()
    exec keys a = do
        post keys a
        putStrLn "::::::::::::2"
        posted def a
        putStrLn "::::::::::::3"
        return ()
    b = Article
        { a_id = ""
        , a_title = "hello"
        , a_link = "hello"
        , a_date = Nothing
        , a_body = ""
        }

