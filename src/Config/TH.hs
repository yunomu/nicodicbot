{-# LANGUAGE TemplateHaskell #-}
module Config.TH
    where

import Language.Haskell.TH

import Config.Parser

construct :: ConfTmp -> DecQ
construct tmp = do
    let name = mkName $ fst tmp
    dataD (cxt []) name [] [recC name []] [''Show]

