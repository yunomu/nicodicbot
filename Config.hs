module Config (Config(..), loadConfig) where

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.CSV
import qualified System.IO.UTF8 as UTF8
import System.IO.Error hiding (try)

data Config = Config {
    cfg_rssuri :: String,
    cfg_keywords :: [String],
    cfg_consumer_key :: String,
    cfg_consumer_secret :: String,
    cfg_access_token :: String,
    cfg_access_token_secret :: String
  } deriving (Show)

type Conf = (String, [String])

loadConfig :: String -> IO Config
loadConfig path = do
    str <- UTF8.readFile path
    case parse config "" str of
      Left err   -> fail $ show err
      Right conf -> return $ makeConfig conf

config :: Parser [Conf]
config = commentLines *> many cfield <* eof
  where
    cfield = field <* commentLines

makeConfig :: [Conf] -> Config
makeConfig conf = Config {
    cfg_rssuri = head $ get "rssuri" "",
    cfg_keywords = get "keywords" "",
    cfg_consumer_key = head $ get "consumer_key" "",
    cfg_consumer_secret = head $ get "consumer_secret" "",
    cfg_access_token = head $ get "access_token" "",
    cfg_access_token_secret = head $ get "access_token_secret" ""
  }
  where
    get :: String -> String -> [String]
    get k def = maybe [] id $ lookup k conf

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

field :: Parser (String,[String])
field = (,) <$> key <*> (sep *> value) <* commentLine

key :: Parser String
key = many1 (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") <* spcs

value :: Parser [String]
value = cv_strings <* spcs

cv_string :: Parser String
cv_string = many1 (noneOf ", \t\r\n") <* spcs

cv_strings :: Parser [String]
cv_strings = cv_list cv_string

cv_list :: Parser a -> Parser [a]
cv_list p = sepBy p $ char ',' <* spcs

