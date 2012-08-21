{-# LANGUAGE OverloadedStrings #-}
module Config.Parser
    ( ConfType(..)
    , ConfTmp
    , loadConfigTmp
    ) where

import Text.Parsec
import Text.Parsec.ByteString
import Control.Applicative hiding (many, (<|>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

data ConfType
    = ConfString
    | ConfURI
    | ConfList ConfType
  deriving (Show)

type ConfLine = (String, ConfType)
type ConfTmp = (String, [ConfLine])

loadConfigTmp :: String -> IO ConfTmp
loadConfigTmp filepath = do
    str <- BS.readFile filepath
    case parse confTmp "" str of
        Left err   -> fail $ show err
        Right conf -> return conf

confTmp :: Parser ConfTmp
confTmp = (,)
    <$> (key <* spcs <* commentLines "--")
    <*> confLines

confLines :: Parser [ConfLine]
confLines = commentLines "--" *> many1 confLine <* eof

confLine :: Parser ConfLine
confLine = (,)
    <$> (spcs1 *> key <* spcs1)
    <*> (confType <* spcs <* commentLines "--")

confType :: Parser ConfType
confType = choice [typeString, typeUri, typeList]
  where
    typeString = string "String" *> return ConfString
    typeUri = string "URI" *> return ConfURI
    typeList = ConfList <$> (char '[' *> confType) <* char ']'

comment :: String -> Parser ()
comment sig = () <$ string sig <* many (noneOf "\n")

commentLine :: String -> Parser ()
commentLine sig = () <$ (comment sig *> newline <|> newline)

commentLines :: String -> Parser ()
commentLines sig = () <$ many (commentLine sig)

key :: Parser String
key = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

spcs :: Parser ()
spcs = () <$ many spc

spcs1 :: Parser ()
spcs1 = () <$ many1 spc

spc :: Parser Char
spc = satisfy (`elem` " \t")

