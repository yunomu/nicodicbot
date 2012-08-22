{-# LANGUAGE OverloadedStrings #-}
module Config
    ( module Config.Internal
    , loadConfig
    ) where

import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec
import Text.Parsec.ByteString
import Data.Default
import qualified Data.ByteString as BS
import Network.URI

import Config.Internal
import Config.Lib

data Conf = RSSURI String
          | Keywords [String]
          | ConsumerKey String
          | ConsumerSecret String
          | AccessToken String
          | AccessTokenSecret String
          | DBHost String
          | DBName String

instance Default Config where
    def = Config{
        rssuri = "",
        keywords = [],
        consumer_key = "",
        consumer_secret = "",
        access_token = "",
        access_token_secret = "",
        db_host = "",
        db_name = ""}

parsers :: [Parser Conf]
parsers = [
    f "rssuri"              RSSURI            cv_uri,
    f "keywords"            Keywords          (cv_list cv_string),
    f "consumer_key"        ConsumerKey       cv_string,
    f "consumer_secret"     ConsumerSecret    cv_string,
    f "access_token"        AccessToken       cv_string,
    f "access_token_secret" AccessTokenSecret cv_string,
    f "dbhost"              DBHost            cv_string,
    f "dbname"              DBName            cv_string]

f :: String -> (a -> Conf) -> Parser a -> Parser Conf
f name c p = c <$> (string name *> spcs *> sep *> p) <* spcs <* commentLine "#"

loadConfig :: String -> IO Config
loadConfig filePath = do
    str <- BS.readFile filePath
    case parse config [] str of
      Left err   -> fail $ show err
      Right conf -> return $ makeConfig conf

makeConfig :: [Conf] -> Config
makeConfig = makeConfig' def
  where
    makeConfig' conf []     = conf
    makeConfig' conf (c:cs) = case c of
        RSSURI a            -> m conf{rssuri = a}
        Keywords a          -> m conf{keywords = a}
        ConsumerKey a       -> m conf{consumer_key = a}
        ConsumerSecret a    -> m conf{consumer_secret = a}
        AccessToken a       -> m conf{access_token = a}
        AccessTokenSecret a -> m conf{access_token_secret = a}
        DBHost a            -> m conf{db_host = a}
        DBName a            -> m conf{db_name = a}
      where
        m cfg = makeConfig' cfg cs

config :: Parser [Conf]
config = commentLines "#" *> many1 confline <* eof
  where
    confline :: Parser Conf
    confline = (choice $ map try parsers) <* commentLines "#"

