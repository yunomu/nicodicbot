{-# LANGUAGE OverloadedStrings #-}
module Config.Parser
    ( loadConfigTmp
    , confTmpParser
    ) where

import Text.Parsec
import Text.Parsec.ByteString
import Control.Applicative hiding (many, (<|>))
import qualified Data.ByteString.Char8 as BC

import Config.Lib
import Config.Types

loadConfigTmp :: String -> IO ConfTmp
loadConfigTmp filepath = do
    str <- readFile filepath
    return $ confTmpParser str

confTmpParser :: String -> ConfTmp
confTmpParser str =
    case parse confTmp "" (BC.pack str) of
        Left err   -> error $ show err
        Right conf -> conf

confTmp :: Parser ConfTmp
confTmp = (,)
    <$> (commentLines *> key <* spcs <* commentLines)
    <*> confLines

confLines :: Parser [ConfLine]
confLines = commentLines *> many1 confLine <* eof

confLine :: Parser ConfLine
confLine = (,)
    <$> (spcs1 *> key <* spcs1)
    <*> (confType <* spcs <* commentLines)

confType :: Parser ConfType
confType = choice [typeString, typeUri, typeList]
  where
    typeString = string "String" *> return ConfString
    typeUri = string "URI" *> return ConfURI
    typeList = ConfList <$> (char '[' *> confType) <* char ']'

