{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Trans.Resource
import qualified Control.Exception.Lifted as E
import Data.Conduit.Attoparsec

import Config
--import Article
import RssParser

curl :: (MonadResource m, MonadBaseControl IO m) =>
    String -> m (ResumableSource m ByteString)
curl url = do
    request <- liftIO $ parseUrl url
    manager <- liftIO $ newManager def
    responseBody <$> http request manager

procArticle :: (MonadResource m, MonadBaseControl IO m)
    => Item -> m ()
procArticle item = do
    body <- (curl $ BC.unpack $ link item) >>= ($$+- CB.take 10240)
    liftIO $ print item

procEntries' :: (MonadResource m, MonadBaseControl IO m) =>
    GLSink ByteString m ()
procEntries' = do
    item <- itemParser
--    resourceForkIO $ procArticle item
--    procArticle item
    procEntries'

procEntries :: (MonadResource m, MonadBaseControl IO m) =>
    ResumableSource m ByteString -> m ()
procEntries src = do
    (src1, item) <- src $$++ itemParser
    --E.handle (const $ return ()) $ procEntries src1
    procEntries src1
{-
  where
    handler :: (MonadResource m, MonadBaseControl IO m) =>
        ParseError e -> m ()
    handler _ = return ()
-}

main :: IO ()
main = do
    config <- getConfig
    let uri = rssuri config
    runResourceT $ curl uri >>= procEntries
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

