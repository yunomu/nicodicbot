{-# LANGUAGE OverloadedStrings #-}
module ArticleParser where

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Text.HTML.DOM as DOM
import Control.Monad.IO.Class
import Data.XML.Types
import Data.Text (Text)
import Safe

awaitWhile :: Monad m
    => (i -> Bool) -> Pipe l i o u m (Maybe i)
awaitWhile f = await >>= maybe (return Nothing) (\i -> if f i
      then return $ Just i
      else awaitWhile f
    )

nicopeName :: Text -> Name 
nicopeName t = Name
    { nameLocalName = t
    , nameNamespace = Nothing
    , namePrefix = Nothing
    }

isBeginTag :: Event -> Bool
isBeginTag (EventBeginElement _ _) = True
isBeginTag _                       = False

isBeginTagName :: Text -> Event -> Bool
isBeginTagName t (EventBeginElement Name{nameLocalName=name} _)
    | t == name    = True
    | otherwise    = False
isBeginTagName _ _ = False

isBeginTagNameAttr :: Text -> [(Text, [Text])] -> Event -> Bool
isBeginTagNameAttr t as (EventBeginElement Name{nameLocalName=name} attrs)
    | t == name && ca as == attrs = True
    | otherwise                   = False
  where
    ca = map $ \(k, v) -> (nicopeName k, map ContentText v)
isBeginTagNameAttr _ _ _ = False

isTagId :: Text -> Event -> Bool
isTagId tid (EventBeginElement _ attrs) = f attrs
  where
    f as = case lookup (nicopeName "id") as >>= headMay of
        Nothing -> False
        Just (ContentText c)  -> c == tid
        Just _  -> False
isTagId _ _ = False

isEndTagName :: Text -> Event -> Bool
isEndTagName t (EventEndElement Name{nameLocalName=name})
    | t == name  = True
    | otherwise  = False
isEndTagName _ _ = False

contents :: MonadThrow m => GLConduit Event m Text
contents = contents' 0
  where
    contents' :: MonadThrow m => Int -> GLConduit Event m Text
    contents' l = do
        me <- await
        case me of
            Nothing -> return ()
            Just (EventContent (ContentText c)) -> yield c >> return ()
            Just (EventBeginElement _ _) -> contents' (l+1)
            Just e@(EventEndElement _) -> if l == 0
                then leftover e >> return ()
                else contents' (l-1)
            Just _ -> contents' l

conduitWhile :: Monad m => (i -> Bool) -> GConduit i m i
conduitWhile f = await >>= mbf
  where
    mbf Nothing     = return ()
    mbf (Just i)
        | f i       = yield i >> conduitWhile f
        | otherwise = return ()

main :: IO ()
main = runResourceT $ do
    let es = CB.sourceFile "test/ume.html" $= DOM.eventConduit
--    doc <- es $= conduitWhile (not . isBeginTag) $$ CL.consume
    doc <- es $= CL.filter (isTagId "article") $= CL.isolate 10 $$ CL.consume
    liftIO $ print doc
    liftIO $ print $ length doc
    return ()

