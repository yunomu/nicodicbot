module Config (Config, rssUri, keyWord, loadConfig) where

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.CSV
import System.IO
import System.IO.Error hiding (try)

data Config = Config {
    rssUri :: String,
    keyWord :: [String]
  } | Error deriving (Show)

loadConfig :: String -> IO Config
loadConfig path = do
    str <- readFile path
    case parse config "" str of
      Left err   -> do print err; return Config.Error
      Right cfg  -> return cfg

config :: Parser Config
config = build <$> uri <*> keywords
  where
    build u ks = Config {rssUri = u, keyWord = ks}

word :: Parser String
word = many1 $ noneOf ",\n"

symbol :: Parser a -> Parser a
symbol p = spaces *> p <* spaces
  where
    spaces = many space

line :: String -> Parser a -> Parser a
line label p = string label *> symbol (char ':') *> p
    <* (optional newline <|> eof)

uri :: Parser String
uri = line "rssuri" word

keywords :: Parser [String]
keywords = line "keywords" $ sepBy1 (symbol word) $ char ','

