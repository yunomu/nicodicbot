{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Config
    ( Config(..)
    , loadConfig
    ) where

import Text.Parsec
import qualified Data.ByteString as BS

import Config.TH

construct "configParser" [config|
Config
    rssuri URI
    keywords [String]
    consumer_key String
    consumer_secret String
    access_token String
    access_token_secret String
    db_host String
    db_name String
|]

loadConfig :: String -> IO Config
loadConfig filepath = do
    str <- BS.readFile filepath
    case parse configParser [] str of
      Left err   -> fail $ show err
      Right conf -> return conf

