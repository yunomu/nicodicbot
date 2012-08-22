{-# LANGUAGE TemplateHaskell #-}
module Config.Internal
    where

import Language.Haskell.TH

import Config.TH
import Config.Parser

runIO (loadConfigTmp "config") >>= construct

