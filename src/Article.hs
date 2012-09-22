{-# LANGUAGE OverloadedStrings #-}

module Article
    ( Article(..)
    , sinkArticle
    , idLink
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Text.HTML.DOM as DOM
import Data.XML.Types
import Data.Attoparsec.Text
import Data.Conduit.Attoparsec
import Control.Applicative
import Control.Monad.State
import Safe
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time

import RssParser

data Article = Article
    { articleId :: ByteString
    , articleTitle :: ByteString
    , articleUpdate :: ZonedTime
    , isMatch :: Bool
    }
  deriving (Show)

data ArticleInfo = AIText Text | AIExist
type ArticleTmp = Map Text ArticleInfo

idLink :: Article -> ByteString
idLink a = "http://dic.nicovideo.jp/id/" <> articleId a

toBS :: Text -> ByteString
toBS = BC.pack . T.unpack

nicopeName :: Text -> Name 
nicopeName t = Name
    { nameLocalName = t
    , nameNamespace = Nothing
    , namePrefix = Nothing
    }

isTagId :: Text -> Event -> Bool
isTagId tid (EventBeginElement _ attrs) = f attrs
  where
    f as = case lookup (nicopeName "id") as >>= headMay of
        Nothing -> False
        Just (ContentText c)  -> c == tid
        Just _  -> False
isTagId _ _ = False

contents :: MonadThrow m => GLConduit Event m Text
contents = contents' 0
  where
    contents' :: MonadThrow m => Int -> GLConduit Event m Text
    contents' l = await >>= maybe (return ()) proc
      where
        proc (EventContent (ContentText c)) = yield c >> contents' l
        proc (EventBeginElement _ _)        = contents' (l+1)
        proc e@(EventEndElement _)          = if l == 0
            then leftover e >> return ()
            else contents' (l-1)
        proc _                              = contents' l

sinkDropWhile :: Monad m => (i -> Bool) -> GLSink i m ()
sinkDropWhile f = await >>= maybe (return ()) g
  where
    g i | f i       = sinkDropWhile f
        | otherwise = leftover i >> return ()

articleIdParser :: StateT ArticleTmp Parser ()
articleIdParser = stateParser
    (string "ページ番号: " *> (T.pack <$> many1 digit))
    (\s a -> Map.insert "articleId" (AIText a) s)

keywordsParser :: [Text] -> StateT ArticleTmp Parser ()
keywordsParser keys = stateParser
    (choice $ string <$> keys)
    (\s _ -> Map.insert "match" AIExist s)

stateParser :: Parser a -> (s -> a -> s) -> StateT s Parser ()
stateParser p f = f <$> get <*> (lift $ try p) >>= put

articleParser :: [Text] -> StateT ArticleTmp Parser ()
articleParser keywords = () <$ many inner
  where
    inner = choice parsers <|> (lift anyChar *> inner)
    parsers =
        [ articleIdParser
        , keywordsParser keywords
        ]

sinkArticle :: MonadThrow m => Item -> [Text] -> Sink ByteString m Article
sinkArticle item keywords = DOM.eventConduit =$ do
    sinkDropWhile (not . isTagId "article")
    CL.drop 1
    contents =$ do
        tmp <- sinkParser (execStateT (articleParser keywords) Map.empty)
        aid <- maybe
            (fail "article id not found")
            (\(AIText aid) -> return $ toBS aid)
            $ Map.lookup "articleId" tmp
        return Article
            { articleId = aid
            , articleTitle = title item
            , articleUpdate = date item
            , isMatch = Map.member "match" tmp
            }
