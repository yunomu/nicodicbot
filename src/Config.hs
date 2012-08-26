module Config (
    Config(..),
    loadConfig
  ) where

import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec hiding (State)
import Text.Parsec.String
import qualified System.IO.UTF8 as UTF8
import Network.URI
import Data.Default
import Control.Monad.State

data Conf = RSSURI String
          | Keywords [String]
          | ConsumerKey String
          | ConsumerSecret String
          | AccessToken String
          | AccessTokenSecret String
          | DBHost String
          | DBName String

data Config = Config {
    cfg_rssuri :: String,
    cfg_keywords :: [String],
    cfg_consumer_key :: String,
    cfg_consumer_secret :: String,
    cfg_access_token :: String,
    cfg_access_token_secret :: String,
    cfg_db_host :: String,
    cfg_db_name :: String}
  deriving (Show)

instance Default Config where
    def = Config{
        cfg_rssuri = "",
        cfg_keywords = [],
        cfg_consumer_key = "",
        cfg_consumer_secret = "",
        cfg_access_token = "",
        cfg_access_token_secret = "",
        cfg_db_host = "",
        cfg_db_name = ""}

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
makeConfig confs = flip execState def $ mapM_ mkConf confs

mkConf :: Conf -> State Config ()
mkConf (RSSURI a) = get >>= \c -> put c{cfg_rssuri = a}
mkConf (Keywords a) = get >>= \c -> put c{cfg_keywords = a}
mkConf (ConsumerKey a) = get >>= \c -> put c{cfg_consumer_key = a}
mkConf (ConsumerSecret a) = get >>= \c -> put c{cfg_consumer_secret = a}
mkConf (AccessToken a) = get >>= \c -> put c{cfg_access_token = a}
mkConf (AccessTokenSecret a) = get >>= \c -> put c{cfg_access_token_secret = a}
mkConf (DBHost a) = get >>= \c -> put c{cfg_db_host = a}
mkConf (DBName a) = get >>= \c -> put c{cfg_db_name = a}

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

