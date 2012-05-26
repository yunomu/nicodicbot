{-# LANGUAGE OverloadedStrings #-}

module Store
    ( DBConfig
    , def
    , store
    ) where

import Database.MongoDB
import Database.MongoDB.Internal.Util
import Data.Default
import Control.Monad.IO.Class
import Data.Conduit
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
    either
        (\f -> fail $ show f)
        (\_ -> return ())
        e

insertArticle :: MonadIO' m => Article -> Action m ()
insertArticle a = do
    doc <- findOne $ select cond table
    maybe
        (insert_ table $ a2doc a)
        (\_ -> return ())
        doc
  where
    cond =
        [ "article_id" =: a_id a
        , "date"       =: zonedTimeToUTC (fromJust $ a_date a)
        ]
    table = "update"

a2doc :: Article -> Document
a2doc a =
    [ "article_id" =: a_id a
    , "title"      =: a_title a
    , "link"       =: a_link a
    , "date"       =: zonedTimeToUTC (fromJust $ a_date a)
    , "posted"     =: True
    ]

