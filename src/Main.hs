{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Concurrent (ThreadId)
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.Chan as Chan
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Network.Http.Client (URL)
import qualified Network.Http.Client as Http
import System.IO
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Concurrent as SC
import System.Environment
import System.Exit

import Config
import Article
import RssParser

curl :: URL -> IO (InputStream ByteString)
curl url = Http.get url $ \_ i -> return i

procArticle :: OutputStream Article -> [Text] -> Item -> IO ()
procArticle os keys item = do
    o <- curl (link item) >>= parseArticle keys item
    Streams.write (Just o) os

procEntries :: [Text] -> InputStream ByteString -> IO ()
procEntries keys src0 = do
    (is, os) <- SC.makeChanPipe
    itemParser src0 >>= f os []
    Streams.filter isMatch is >>= dump
  where
    f os rs src = do
        mi <- Streams.read src
        case mi of
            Nothing -> do
--                _ <- sequence $ map fst rs
                return ()
            Just i  -> do
                r <- thread $ procArticle os keys i
                f os (r:rs) src
    dump is = do
        ma <- Streams.read is
        case ma of
            Nothing -> return ()
            Just a  -> do
                BC.putStr $ mconcat [articleTitle a, ",", idLink a, ","]
                dump is

thread :: IO a -> IO (IO a, ThreadId)
thread action = do
    ch <- Chan.newChan
    th <- CC.forkFinally action $ Chan.writeChan ch
    return (Chan.readChan ch >>= either throwIO return, th)

main :: IO ()
main = do
    config <- getConfig
    let uri = rssuri config
    let keys = keywords config
    curl (BC.pack uri) >>= procEntries (map T.pack keys)
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
