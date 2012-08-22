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
    let var = varP n
    bindS var $ appE (confParser ctype) [|name|]

confParser :: ConfType -> ExpQ
confParser ConfString = appE (varE 'val) (varE 'cv_string)
confParser ConfURI = appE (varE 'val) (varE 'cv_uri)
confParser (ConfList ctype) =
    appE (varE 'val) $ appE (varE 'cv_list) (confParser ctype)

{-
parser :: Parser Config
parser = do
    string "rssuri"
    val <- cv_string
    return $ Config {rssuri = val}

[ValD
 (VarP p_0)
 (NormalB
  (DoE
   [BindS
     (VarP val_1)
     (AppE
      (AppE
       (VarE Config.Lib.val)
       (VarE Config.Lib.cv_string))
      (LitE (StringL "rssuri"))),
    NoBindS
     (AppE
      (VarE GHC.Base.return)
      (RecConE Config.Internal.Config [(Config.Internal.rssuri,VarE val_1)]))]
  )) []]

-}

