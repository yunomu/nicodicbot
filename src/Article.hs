{-# LANGUAGE OverloadedStrings #-}
module Article
    ( Article(..)
    , sinkArticle
    ) where

import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Text.HTML.DOM as DOM
import Data.XML.Types
import Data.Text (Text)
import qualified Data.Text as T
import Safe
import Data.Attoparsec.Text
import Control.Applicative
import Data.Conduit.Attoparsec
import Data.Default
import Control.Monad.State

data Article = Article
    { articleId :: Maybe Text
    , isMatch :: Bool
    }
  deriving (Show)

instance Default Article where
    def = Article
        { articleId = Nothing
        , isMatch = False
        }

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

articleIdParser :: StateT Article Parser ()
articleIdParser = stateParser
    (string "ページ番号: " *> (T.pack <$> many1 digit))
    (\s a -> s{articleId = Just a})

keywordsParser :: [Text] -> StateT Article Parser ()
keywordsParser keys = stateParser
    (choice $ string <$> keys)
    (\s _ -> s{isMatch = True})

stateParser :: Parser a -> (s -> a -> s) -> StateT s Parser ()
stateParser p f = f <$> get <*> (lift $ try p) >>= put

articleParser :: [Text] -> StateT Article Parser ()
articleParser keywords = () <$ many inner
  where
    inner = choice parsers <|> (lift anyChar *> inner)
    parsers =
        [ articleIdParser
        , keywordsParser keywords
        ]

sinkArticle :: MonadThrow m => [Text] -> Sink ByteString m Article
sinkArticle keywords = DOM.eventConduit =$ do
    sinkDropWhile (not . isTagId "article")
    CL.drop 1
    contents =$ sinkParser (execStateT (articleParser keywords) def)
