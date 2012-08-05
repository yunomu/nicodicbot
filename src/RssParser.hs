{-# LANGUAGE OverloadedStrings #-}
module RssParser
    (Item(..), itemParser) where

import Control.Applicative
import Data.Attoparsec
import Data.ByteString
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import Data.Conduit.Attoparsec (sinkParser)

data Item
    = Item
        { title :: ByteString
        , link :: ByteString
        , date :: ByteString
        } deriving Show


itemParser :: MonadThrow m => GLSink ByteString m Item
itemParser = sinkParser item

item :: Parser Item
item = tag "item" $ do
    aTitle <- tag "title" next
    aLink <- tag "link" next
    tag "description" next
    aDate <- tag "dc:date" next
    return $ Item {title = aTitle, link = aLink, date = aDate}

tag :: ByteString -> Parser a -> Parser a
tag str p = do
    inFix $ string $ BC.concat ["<", str]
    many space
    inFix $ string ">"
    ret <- p
    inFix $ string $ BC.concat ["</", str, ">"]
    next
    return ret

next :: Parser ByteString
next = pack <$> (many $ satisfy $ notf $ inClass "<")

notf :: (a -> Bool) -> a -> Bool
notf f v = not $ f v

space :: Parser ()
space = skip $ inClass " \t\r\n"

inFix :: Parser a -> Parser a
inFix p = try p <|> anyWord8 *> inFix p

