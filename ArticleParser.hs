module ArticleParser (Article, getArticle) where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike (StringLike, toString, fromString)

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
      ++ ", body = \"" ++ body a ++ "\"}"

getArticle :: String -> String -> Either String Article
getArticle atitle content = do
    aid <- getId tags
    adate <- getDate tags
    abody <- getBody tags
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

getId :: StringLike str => [Tag str] -> Either String String
getId ts = getTagText "a" (tos f) ts
  where
    f n v = n == "href" && take 4 v == "/id/"

getDate :: StringLike str => [Tag str] -> Either String String
getDate ts = getTagText "span" (tos f) ts
  where
    f n v = n == "style" && v == "color:red;"

getBody :: (StringLike str, Show str) =>
    [Tag str] -> Either String String
getBody tags = Right $ plane $ search "div" "article" $ tagTree tags

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

    matchAttrId :: StringLike str => [Attribute str] -> String -> Bool
    matchAttrId [] _ = False
    matchAttrId ((n,v):as) aid
      | toString n == "id" && toString v == aid = True
      | otherwise = matchAttrId as aid

{-
main :: IO ()
main = do
    str <- readFile "test/ume.html"
    print $ getArticle "ume" str
---}

