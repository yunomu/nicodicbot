{-# LANGUAGE OverloadedStrings #-}
module Twitter
    ( TwKeys
    , twKeys
    , post
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Codec.Binary.UTF8.String (encodeString)

import Article

data TwKeys = TK {
    consumer_key :: ByteString,
    consumer_secret :: ByteString,
    access_token :: ByteString,
    access_token_secret :: ByteString}
  deriving (Show)

twKeys :: String -> String -> String -> String -> TwKeys
twKeys ck cs at ats = TK {
    consumer_key = BC.pack ck,
    consumer_secret = BC.pack cs,
    access_token = BC.pack at,
    access_token_secret = BC.pack ats}

post :: TwKeys -> Article -> IO L.ByteString
post keys a = runResourceT $ do
    manager <- liftIO $ newManager def
    req <- liftIO $ parseUrl "http://twitter.com/statuses/update.json"
    request <- signOAuth oauth credential $ postMessage status req
    response <- http request manager
    responseBody response $$ CB.take 1024
  where
    credential = newCredential (access_token keys) (access_token_secret keys)
    site = "https://api.twitter.com"
    oauth = newOAuth {
        oauthRequestUri = site ++ "/oauth/request_token",
        oauthAccessTokenUri = site ++ "/oauth/access_token",
        oauthAuthorizeUri = site ++ "/oauth/authorize",
        oauthConsumerKey = consumer_key keys,
        oauthConsumerSecret = consumer_secret keys}
    status = BC.pack $ encodeString $ a_title a ++ " :: " ++ a_link a

    postMessage :: MonadUnsafeIO m => ByteString -> Request m -> Request m
    postMessage msg req = urlEncodedBody [("status", msg)] req

