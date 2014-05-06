{-# LANGUAGE DeriveDataTypeable, RankNTypes #-}

module Article
    ( Article(..)
    , ArticleParseException(..)
    , parseArticle
    , idLink
    , ignoreArticle
    ) where

import Control.Applicative
import Control.Exception.Lifted as E
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadThrow, runResourceT)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Text as AT
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Conduit (Conduit, Producer, Consumer, (=$), ($$))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Conduit.Attoparsec (sinkParser)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Typeable (Typeable)
import Data.XML.Types
import Safe
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified Text.HTML.DOM as DOM

import RssParser

data ArticleParseException
    = ArticleIdNotFound
  deriving (Show, Typeable)

instance Exception ArticleParseException

data Article = Article
    { articleId :: ByteString
    , articleTitle :: ByteString
    , articleUpdate :: ZonedTime
    , isMatch :: Bool
    }
  deriving (Show)

ignoreArticle :: Monad m => ArticleParseException -> m ()
ignoreArticle _ = return ()

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

contents :: MonadThrow m => Conduit Event m Text
contents = contents' 0
  where
    contents' :: MonadThrow m => Int -> Conduit Event m Text
    contents' l = C.await >>= maybe (return ()) proc
      where
        proc (EventContent (ContentText c)) = C.yield c >> contents' l
        proc (EventBeginElement _ _)        = contents' (l+1)
        proc e@(EventEndElement _)          = if l == 0
            then C.leftover e >> return ()
            else contents' (l-1)
        proc _                              = contents' l

sinkDropWhile :: Monad m => (i -> Bool) -> Consumer i m ()
sinkDropWhile f = C.await >>= maybe (return ()) g
  where
    g i | f i       = sinkDropWhile f
        | otherwise = C.leftover i >> return ()

articleIdParser :: StateT ArticleTmp Parser ()
articleIdParser = stateParser
    (A.string "ページ番号: " *> (T.pack <$> A.many1 A.digit))
    (\s a -> Map.insert "articleId" (AIText a) s)

keywordsParser :: [Text] -> StateT ArticleTmp Parser ()
keywordsParser keys = stateParser
    (A.choice $ A.string <$> keys)
    (\s _ -> Map.insert "match" AIExist s)

stateParser :: Parser a -> (s -> a -> s) -> StateT s Parser ()
stateParser p f = f <$> State.get <*> (lift $ AT.try p) >>= State.put

articleParser :: [Text] -> StateT ArticleTmp Parser ()
articleParser keywords = () <$ many inner
  where
    inner = A.choice parsers <|> (lift A.anyChar *> inner)
    parsers =
        [ articleIdParser
        , keywordsParser keywords
        ]

streamToProducer :: MonadIO m => InputStream a -> Producer m a
streamToProducer is = liftIO (Streams.read is) >>=
    maybe (return ()) (\a -> C.yield a >> streamToProducer is)

parseArticle :: [Text] -> Item -> InputStream ByteString -> IO Article
parseArticle keywords item is = runResourceT
    $ streamToProducer is $$ DOM.eventConduit =$ do
        sinkDropWhile (not . isTagId "article")
        CL.drop 1
        contents =$ do
            tmp <- sinkParser
                $ State.execStateT (articleParser keywords) Map.empty
            aid <- maybe
                (E.throw ArticleIdNotFound)
                (\(AIText aid) -> return $ toBS aid)
                $ Map.lookup "articleId" tmp
            return Article
                { articleId = aid
                , articleTitle = title item
                , articleUpdate = date item
                , isMatch = Map.member "match" tmp
                }
