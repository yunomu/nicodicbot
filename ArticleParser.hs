module ArticleParser (Article(..), getArticle, strContain) where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike (StringLike, toString, fromString)
import Data.Attoparsec.Text
import Data.Text (Text, pack)
import Control.Applicative

data Article = Article {
    articleId :: String,
    title :: String,
    link :: String,
    date :: String,
    body :: String}

instance Show Article where
    show a = "A {id = " ++ articleId a
      ++ ", title = " ++ title a
      ++ ", link = " ++ link a
      ++ ", date = \"" ++ date a
      ++ "\", body = \"" ++ Prelude.take 20 (body a) ++ "...\"}"

getArticle :: String -> String -> Either String Article
getArticle atitle content = do
    adate <- getDate tags
    abody <- getBody tags
    aid <- getId abody
    return Article {
      articleId = aid,
      title = atitle,
      link = "http://dic.nicovideo.jp/id/" ++ aid,
      date = adate,
      body = abody}
  where
    tags = parseTags content

isTag :: StringLike str =>
    Tag str -> String -> ([Attribute str] -> Bool) -> Bool
isTag (TagOpen tag attrs) str f
  | (toString tag) == str = f attrs
  | otherwise             = False
isTag _ _ _               = False

isAttr :: StringLike str =>
    (Attribute str -> Bool) -> [Attribute str] -> Bool
isAttr _ []   = False
isAttr f ((n,v):as)
  | f (n, v)  = True
  | otherwise = False

text :: StringLike str => [Tag str] -> String
text ((TagText str):ts) = toString str

getTagText :: StringLike str =>
    String -> (Attribute str -> Bool) -> [Tag str]
      -> Either String String
getTagText tag _ []     = Left $ "tag not found: " ++ tag
getTagText tag f (t:ts) =
    if isTag t tag (isAttr f) then
      Right $ text ts
     else
      getTagText tag f ts

tos :: StringLike str =>
    (String -> String -> Bool) -> Attribute str -> Bool
tos f (n,v) = f (toString n) (toString v)

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

getDate :: StringLike str => [Tag str] -> Either String String
getDate ts = getTagText "span" (tos f) ts
  where
    f n v = n == "style" && v == "color:red;"

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
      | otherwise                = search' (children ++ ts)
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
  case parse (contain keys) (pack $ body article) of
    Fail _ _ _ -> False
    Partial _  -> False
    Done _ _   -> True
  where
    contain :: [String] -> Parser Text
    contain ks = inFix $ choice $ string <$> map pack keys

