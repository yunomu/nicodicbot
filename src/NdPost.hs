module Main where

import System.IO
import System.Environment
import System.Exit

import Config
import Store
import Twitter

main :: IO ()
main = do
    config <- getConfig
    articles <- unpostedArticles def
    let keys = twKeys
          (cfg_consumer_key config)
          (cfg_consumer_secret config)
          (cfg_access_token config)
          (cfg_access_token_secret config)
    mapM_ (post keys) articles
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

