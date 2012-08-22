{-# LANGUAGE OverloadedStrings #-}
module Config.Parser
    ( loadConfigTmp
    ) where

import Text.Parsec
import Text.Parsec.ByteString
import Control.Applicative hiding (many, (<|>))
import qualified Data.ByteString as BS

import Config.Lib
import Config.Types

loadConfigTmp :: String -> IO ConfTmp
loadConfigTmp filepath = do
    str <- BS.readFile filepath
    case parse confTmp "" str of
        Left err   -> fail $ show err
        Right conf -> return conf

confTmp :: Parser ConfTmp
confTmp = (,)
    <$> (key <* spcs <* commentLines)
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

