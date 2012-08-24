{-# LANGUAGE OverloadedStrings #-}
module Config
    ( module Config.Internal
    , loadConfig
    ) where

import Text.Parsec
import qualified Data.ByteString as BS

import Config.Internal

loadConfig :: String -> IO Config
loadConfig filepath = do
    str <- BS.readFile filepath
    case parse configParser [] str of
      Left err   -> fail $ show err
      Right conf -> return conf

