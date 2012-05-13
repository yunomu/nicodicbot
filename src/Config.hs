module Config (
    Config(..),
    loadConfig
  ) where

import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec
import Text.Parsec.String
import qualified System.IO.UTF8 as UTF8
import Network.URI
import Data.Default

data Conf = RSSURI String
          | Keywords [String]
          | ConsumerKey String
          | ConsumerSecret String
          | AccessToken String
          | AccessTokenSecret String

data Config = Config {
    cfg_rssuri :: String,
    cfg_keywords :: [String],
    cfg_consumer_key :: String,
    cfg_consumer_secret :: String,
    cfg_access_token :: String,
    cfg_access_token_secret :: String}
  deriving (Show)

instance Default Config where
    def = Config{
        cfg_rssuri = "",
        cfg_keywords = [],
        cfg_consumer_key = "",
        cfg_consumer_secret = "",
        cfg_access_token = "",
        cfg_access_token_secret = ""}

parsers :: [Parser Conf]
parsers = [
    f "rssuri"              RSSURI            cv_uri,
    f "keywords"            Keywords          (cv_list cv_string),
    f "consumer_key"        ConsumerKey       cv_string,
    f "consumer_secret"     ConsumerSecret    cv_string,
    f "access_token"        AccessToken       cv_string,
    f "access_token_secret" AccessTokenSecret cv_string]
  where
    f :: String -> (a -> Conf) -> Parser a -> Parser Conf
    f name c p = c <$> (string name *> spcs *> sep *> p) <* spcs <* commentLine

loadConfig :: String -> IO Config
loadConfig filePath = do
    str <- UTF8.readFile filePath
    case parse config "" str of
      Left err   -> fail $ show err
      Right conf -> return $ makeConfig conf

makeConfig :: [Conf] -> Config
makeConfig = makeConfig' def
  where
    makeConfig' conf []     = conf
    makeConfig' conf (c:cs) = case c of
        RSSURI a            -> m conf{cfg_rssuri = a}
        Keywords a          -> m conf{cfg_keywords = a}
        ConsumerKey a       -> m conf{cfg_consumer_key = a}
        ConsumerSecret a    -> m conf{cfg_consumer_secret = a}
        AccessToken a       -> m conf{cfg_access_token = a}
        AccessTokenSecret a -> m conf{cfg_access_token_secret = a}
      where
        m cfg = makeConfig' cfg cs

config :: Parser [Conf]
config = commentLines *> many1 confline <* eof
  where
    confline :: Parser Conf
    confline = (choice $ map try parsers) <* commentLines

comment :: Parser ()
comment = () <$ char '#' <* many (noneOf "\n")

commentLine :: Parser ()
commentLine = () <$ (comment *> newline <|> newline)

commentLines :: Parser ()
commentLines = () <$ many commentLine

sep :: Parser ()
sep = () <$ char ':' *> spcs

spcs :: Parser ()
spcs = () <$ many spc

spc :: Parser Char
spc = satisfy (`elem` " \t")

cv_string :: Parser String
cv_string = many1 (noneOf ", \t\r\n") <* spcs

cv_list :: Parser p -> Parser [p]
cv_list p = sepBy p (spcs *> char ',' <* spcs) <* spcs

cv_uri :: Parser String
cv_uri = do
    str <- cv_string
    if isURI str then return str else fail "parse error: URI"

