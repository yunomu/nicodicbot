module ArticleParser (Article, getArticle) where

import Text.HTML.TagSoup
import Text.StringLike (StringLike, toString)

data Article = Article {
    articleId :: String,
    title :: String,
    link :: String,
    date :: String}

instance Show Article where
    show a = "A {id = " ++ articleId a
      ++ ", title = " ++ title a
      ++ ", link = " ++ link a
      ++ ", date = \"" ++ date a ++ "\"}"

getArticle :: String -> String -> Either String Article
getArticle atitle content = do
    aid <- getId tags
    adate <- getDate tags
    return Article {
      articleId = aid,
      title = atitle,
      link = "http://dic.nicovideo.jp/id/" ++ aid,
      date = adate}
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

getId :: StringLike str => [Tag str] -> Either String String
getId ts = getTagText "a" (tos f) ts
  where
    f n v = n == "href" && take 4 v == "/id/"

getDate :: StringLike str => [Tag str] -> Either String String
getDate ts = getTagText "span" (tos f) ts
  where
    f n v = n == "style" && v == "color:red;"

{-
main :: IO ()
main = do
    str <- readFile "test/error.html"
    print $ getArticle "ume" str
---}

