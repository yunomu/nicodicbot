module Article (Article(..), getArticle, strContain, dump) where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike (StringLike, toString)
import Data.Attoparsec.Text
import Data.Text (Text, pack)
import Control.Applicative
import Data.Time

data Article = Article {
    a_id :: String,
    a_title :: String,
    a_link :: String,
    a_date :: Maybe ZonedTime,
    a_body :: String}

instance Show Article where
    show a = "A {id = " ++ a_id a
      ++ ", title = " ++ a_title a
      ++ ", link = " ++ a_link a
      ++ ", date = \"" ++ maybe "" show (a_date a)
      ++ "\", body = \"" ++ Prelude.take 20 (a_body a) ++ "...\"}"

dump :: Article -> String
dump a = join [a_title a, a_link a, maybe "" show $ a_date a]
  where
    join []     = error "dump error"
    join [f]    = f
    join (f:fs) = f ++ "," ++ join fs

getArticle :: String -> Either String Article
getArticle content = do
    adate <- Right Nothing
    abody <- getBody tags
    aid <- getId abody
    return Article {
      a_id = aid,
      a_title = "",
      a_link = "http://dic.nicovideo.jp/id/" ++ aid,
      a_date = adate,
      a_body = abody}
  where
    tags = parseTags content

getId :: String -> Either String String
getId article = case parse (inFix artId) (pack article) of
    Fail _ _ msg -> Left msg
    Partial _    -> Left "error"
    Done _ r     -> Right r
  where
    artId :: Parser String
    artId = string (pack "ページ番号: ") *> many1 digit

inFix :: Parser a -> Parser a
inFix p = try p <|> (anyChar *> inFix p)

getArticleTree :: (StringLike str, Show str) =>
    [Tag str] -> [TagTree str]
getArticleTree = search "div" "article" . tagTree

getBody :: (StringLike str, Show str) =>
    [Tag str] -> Either String String
getBody tags = Right $ plane $ getArticleTree tags

plane :: (StringLike str) => [TagTree str] -> String
plane tree = plane' "" $ flattenTree tree
  where
    plane' :: StringLike str => String -> [Tag str] -> String
    plane' r []                 = r
    plane' r ((TagText str):ts) = plane' (r ++ toString str) ts
    plane' r (_:ts)             = plane' r ts

search :: (StringLike str) =>
    String -> String -> [TagTree str] -> [TagTree str]
search tag attrid trees = search' trees
  where
    search' :: StringLike str =>
        [TagTree str] -> [TagTree str]
    search' [] = []
    search' ((TagBranch tagname as children):ts)
      | toString tagname == tag
        && matchAttrId as attrid = children
      | otherwise                = search' $ children ++ ts
    search' (_:ts) = search' ts

    matchAttrId :: StringLike str =>
        [Attribute str] -> String -> Bool
    matchAttrId [] _ = False
    matchAttrId ((n,v):as) aid
      | toString n == "id"
       && toString v == aid = True
      | otherwise           = matchAttrId as aid

strContain :: [String] -> Article -> Bool
strContain keys article =
  case parse (contain keys) (pack $ a_body article) of
    Done _ _   -> True
    Fail _ _ _ -> False
    Partial _  -> False
  where
    contain :: [String] -> Parser Text
    contain ks = inFix $ choice $ string <$> map pack ks

