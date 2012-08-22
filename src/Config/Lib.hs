module Config.Lib
    where

import Text.Parsec
import Text.Parsec.ByteString
import Control.Applicative hiding (many, (<|>))
import Network.URI

comment :: Parser ()
comment = () <$ string "--" <* many (noneOf "\n")

commentLine :: Parser ()
commentLine = () <$ (comment *> newline <|> newline)

commentLines :: Parser ()
commentLines = () <$ many commentLine

key :: Parser String
key = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

spcs :: Parser ()
spcs = () <$ many spc

spcs1 :: Parser ()
spcs1 = () <$ many1 spc

spc :: Parser Char
spc = satisfy (`elem` " \t")

val :: Parser a -> String -> Parser a
val p name = (string name *> spcs *> sep *> p) <* spcs <* commentLine

cv_string :: Parser String
cv_string = many1 (noneOf ", \t\r\n") <* spcs

cv_list :: Parser p -> Parser [p]
cv_list p = sepBy p (spcs *> char ',' <* spcs) <* spcs

cv_uri :: Parser String
cv_uri = do
    str <- cv_string
    if isURI str
      then return str
      else fail "parse error: URI"

sep :: Parser ()
sep = () <$ char ':' *> spcs

