{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.IO
import System.Environment
import System.Exit
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import Network.HTTP.Conduit
import Data.Functor
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import qualified Control.Exception.Lifted as E
import Data.Conduit.Attoparsec
import Data.Text (Text)
import qualified Data.Text as T
import Control.Concurrent.Event
import Data.Monoid

import Config
import Article
import RssParser

curl :: (MonadResource m, MonadBaseControl IO m) =>
    String -> m (ResumableSource m ByteString)
curl url = do
    request <- liftIO $ parseUrl url
    manager <- liftIO $ newManager conduitManagerSettings
    responseBody <$> http request manager

ignoreHttp :: Monad m => HttpException -> m ()
ignoreHttp _ = return ()

procArticle :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -> Item -> m ()
procArticle keys item = E.handle ignoreArticle $ E.handle ignoreHttp $ do
    a <- (curl $ BC.unpack $ link item) >>= ($$+- sinkArticle item keys)
    if isMatch a
        then storeArticle a
        else return ()

storeArticle :: (MonadResource m, MonadBaseControl IO m)
    => Article -> m ()
storeArticle a = do
    liftIO $ do
        BC.putStr $ mconcat [articleTitle a, ",", idLink a, ","]
        print $ articleUpdate a

procEntries :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -> ResumableSource m ByteString -> m ()
procEntries keys src0 = procEntries' src0 []
  where
    procEntries' src es = do
        (src1, item) <- src $$++ itemParser
        e <- forkProc $ procArticle keys item
        let nes = e:es
        E.handle (ignoreParser nes) $ procEntries' src1 nes
    forkProc proc = do
        event <- liftIO new
        runResourceT $ resourceForkIO $ withEvent event proc
        return event
    withEvent event proc = E.finally proc $ liftIO $ set event

ignoreParser :: (MonadResource m, MonadBaseControl IO m)
    => [Event] -> ParseError -> m ()
ignoreParser es _ = liftIO $ mapM_ wait es

main :: IO ()
main = do
    config <- getConfig
    let uri = rssuri config
    let keys = keywords config
    runResourceT $ curl uri >>= procEntries (map T.pack keys)
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

