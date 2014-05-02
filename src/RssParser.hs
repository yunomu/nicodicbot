{-# LANGUAGE OverloadedStrings #-}
module RssParser
    (Item(..), itemParser) where

import Control.Applicative
import Data.Attoparsec
import Data.ByteString
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import Data.Conduit.Attoparsec (sinkParser)
import Data.Time
import System.Locale

--import ParserLib

data Item = Item {
    title :: ByteString,
    link :: ByteString,
    date :: ZonedTime
  } deriving Show


itemParser :: MonadThrow m => Consumer ByteString m Item
itemParser = sinkParser item

item :: Parser Item
item = tag "item" $ do
    aTitle <- tag "title" next
    aLink <- tag "link" next
    tag "description" next
    maDate <- parseDate <$> tag "dc:date" next
    aDate <- maybe (fail "date parse error") return maDate
    return $ Item {title = aTitle, link = aLink, date = aDate}
  where
    parseDate :: ParseTime t => ByteString -> Maybe t
    parseDate = parseTime defaultTimeLocale fmt . BC.unpack
    fmt = "%FT%X%z"

tag :: ByteString -> Parser a -> Parser a
tag str p = do
    inFix $ string $ BC.concat ["<", str]
    many space
    inFix $ string ">"
    ret <- p
    inFix $ string $ BC.concat ["</", str, ">"]
    next
    return ret

space :: Parser ()
space = skip $ inClass " \t\r\n"

next :: Parser ByteString
next = pack <$> (many $ satisfy $ notf $ inClass "<")

notf :: (a -> Bool) -> a -> Bool
notf f v = not $ f v

inFix :: Parser a -> Parser a
inFix p = try p <|> anyWord8 *> inFix p

