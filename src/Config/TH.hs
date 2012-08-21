module Config.TH
    where

import Language.Haskell.TH

import Config.Parser

construct :: ConfTmp -> DecQ
construct tmp = do
    let name = mkName $ fst tmp
    let confLines = snd tmp
    let sType = varStrictType (mkName "name") $ strictType notStrict [t|String|]
    let rec = recC name [sType]
    dataD (cxt []) name [] [rec] [''Show]

