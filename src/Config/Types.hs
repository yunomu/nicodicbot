module Config.Types
    ( ConfType(..)
    , ConfLine
    , ConfTmp
    ) where

data ConfType
    = ConfString
    | ConfURI
    | ConfList ConfType
  deriving (Show)

type ConfLine = (String, ConfType)

type ConfTmp = (String, [ConfLine])

