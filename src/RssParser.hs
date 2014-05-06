module RssParser
    ( Item(..)
    , itemParser
    ) where

import Control.Applicative
import Data.Attoparsec (Parser)
import qualified Data.Attoparsec as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Time (ZonedTime, ParseTime, parseTime)
import System.IO.Streams (InputStream)
import qualified System.IO.Streams.Attoparsec as SA
import System.Locale (defaultTimeLocale)

data Item = Item {
    title :: ByteString,
    link :: ByteString,
    date :: ZonedTime
  } deriving Show

itemParser :: InputStream ByteString -> IO (InputStream Item)
itemParser = SA.parserToInputStream p
  where
    p = (A.endOfInput >> return Nothing) <|> (Just <$> item)

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
    inFix $ A.string $ BC.concat ["<", str]
    many space
    inFix $ A.string ">"
    ret <- p
    inFix $ A.string $ BC.concat ["</", str, ">"]
    next
    return ret

space :: Parser ()
space = A.skip $ A.inClass " \t\r\n"

next :: Parser ByteString
next = BS.pack <$> (many $ A.satisfy $ notf $ A.inClass "<")

notf :: (a -> Bool) -> a -> Bool
notf f v = not $ f v

inFix :: Parser a -> Parser a
inFix p = A.try p <|> A.anyWord8 *> inFix p

