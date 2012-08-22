{-# LANGUAGE TemplateHaskell #-}
module Config.TH
    ( construct
    ) where

import Language.Haskell.TH
import Control.Applicative

import Config.Types
import Config.Lib

construct :: ConfTmp -> DecsQ
construct (nameStr, confLines) = do
    let recName = mkName nameStr
    let rec = recC recName $ map confVSType confLines
    (:[]) <$> dataD (cxt []) recName [] [rec] [''Show]

confVSType :: ConfLine -> VarStrictTypeQ
confVSType (name, ctype) = confVSType' name $ confTypeQ ctype

confTypeQ :: ConfType -> TypeQ
confTypeQ ConfString = [t|String|]
confTypeQ ConfURI = [t|String|]
confTypeQ (ConfList ctype) = appT listT $ confTypeQ ctype

confVSType' :: String -> TypeQ -> VarStrictTypeQ
confVSType' name typeq =
    varStrictType (mkName name) $ strictType notStrict typeq

mkParser :: ConfTmp -> DecQ
mkParser (n, cl) = do
    let recName = mkName n
    funcPat <- varP $ mkName "configParser"
    undefined

confBind :: ConfLine -> StmtQ
confBind (name, ctype) = do
    n <- newName "x"
    let pat = varP n
    undefined

confVarQ :: ConfType -> ExpQ
confVarQ ConfString = varE 'stringVal
confVarQ ConfURI = varE 'uriVal
confVarQ (ConfList ctype) = undefined --varE 'listVal

{-
parser :: Parser Config
parser = do
    string "rssuri"
    val <- cv_string
    return $ Config {rssuri = val}
-}

