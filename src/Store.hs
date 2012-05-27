{-# LANGUAGE OverloadedStrings #-}
module Store
    ( DBConfig
    , def
    , store
    , unpostedArticles
    , posted
    ) where

import Prelude hiding (lookup)
import Database.MongoDB
import Database.MongoDB.Internal.Util
import Control.Monad.IO.Class
import Data.Default
import Data.Conduit hiding (sequence)
import Data.Maybe
import Data.Time

import Article

data DBConfig = DBConfig
    { dbHost :: String
    , dbName :: String
    } deriving (Show)

instance Default DBConfig where
    def = DBConfig
        { dbHost = "127.0.0.1"
        , dbName = "ndbot"
        }

store :: DBConfig -> [Article] -> IO ()
store dbConfig as = runResourceT . liftIO $ do
    pipe <- runIOE $ connect $ host $ dbHost dbConfig
    e <- access pipe master (u $ dbName dbConfig) $ mapM_ insertArticle as
    result e

insertArticle :: MonadIO' m => Article -> Action m ()
insertArticle a = do
    doc <- findOne $ select (cond a) table
    maybe
        (insert_ table $ a2doc a)
        (\_ -> return ())
        doc

cond :: Article -> Selector
cond a =
    [ "article_id" =: a_id a
    , "date"       =: zonedTimeToUTC (fromJust $ a_date a)
    ]

unpostedArticles :: DBConfig -> IO [Article]
unpostedArticles dbConfig = runResourceT . liftIO $ do
    pipe <- runIOE $ connect $ host $ dbHost dbConfig
    e <- access pipe master (u $ dbName dbConfig) $ findUnposted
    r <- result e
    sequence $ map doc2a r
  where
    findUnposted :: Action IO [Document]
    findUnposted = rest =<< find (select ["posted" =: False] table)

posted :: DBConfig -> Article -> IO ()
posted dbConfig article = runResourceT . liftIO $ do
    pipe <- runIOE $ connect $ host $ dbHost dbConfig
    e <- access pipe master (u $ dbName dbConfig) $ update article
    return ()
  where
    update :: Article -> Action IO ()
    update a = modify (Select {selector = cond a, coll = table}) ["posted" =: True]

a2doc :: Article -> Document
a2doc a =
    [ "article_id" =: a_id a
    , "title"      =: a_title a
    , "link"       =: a_link a
    , "date"       =: zonedTimeToUTC (fromJust $ a_date a)
    , "posted"     =: False
    ]

doc2a :: Document -> IO Article
doc2a d = do
    timeZone <- getCurrentTimeZone
    maybe (fail "getCurrentTimeZone") (\a -> return a) $ do
        aid    <- lookup "article_id" d
        atitle <- lookup "title" d
        alink  <- lookup "link" d
        adate  <- lookup "date" d
        return $ Article
            { a_id = aid
            , a_title = atitle
            , a_link = alink
            , a_date = Just $ utcToZonedTime timeZone adate
            , a_body = ""
            }

result :: MonadIO' m => Either Failure a -> m a
result e = either (\f -> fail $ show f) (\a -> return a) e

table :: Collection
table = "update"

