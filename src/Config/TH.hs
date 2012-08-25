{-# LANGUAGE TemplateHaskell #-}
--module Config.TH (construct) where
module Config.TH where

import Language.Haskell.TH
import Control.Applicative
import Text.Parsec.ByteString (Parser)

import Config.Types
import Config.Lib

construct :: String -> ConfTmp -> DecsQ
construct name tmp = (:) <$> mkRecord tmp <*> mkParser name tmp

{-
レコードを作る。
-}
mkRecord :: ConfTmp -> DecQ
mkRecord (nameStr, confLines) =
    dataD (cxt []) recName [] [rec] [''Show]
  where
    recName :: Name
    recName = mkName nameStr

    rec :: ConQ
    rec = recC recName $ map confVSType confLines

confVSType :: ConfLine -> VarStrictTypeQ
confVSType (name, ctype) = confVSType' name $ confTypeQ ctype

confTypeQ :: ConfType -> TypeQ
confTypeQ ConfString = [t|String|]
confTypeQ ConfURI = [t|String|]
confTypeQ (ConfList ctype) = appT listT $ confTypeQ ctype

confVSType' :: String -> TypeQ -> VarStrictTypeQ
confVSType' name typeq =
    varStrictType (mkName name) $ strictType notStrict typeq

{-
こういうのを作る
configParser :: Parser Config
configParser = do
    val1 <- val cv_string "field1"
    val2 <- val cv_uri "field2"
    val3 <- val cv_int "field3"
    return $ Record {field1 = val, field2 = val2, field3 = val3}
-}
mkParser :: String -> ConfTmp -> DecsQ
mkParser name (n, cl) = do
    let recName = mkName n
    let funcName = mkName name
    s <- sigD funcName $ appT (conT ''Parser) (conT recName)
    (binds, cons) <- unzip <$> mapM confBind cl
    let consRec = appE (varE 'return) $ recConE recName cons
    let body = doE (binds ++ [noBindS consRec])
    v <- valD (varP funcName) (normalB body) []
    return [s, v]

{-
    val1 <- val cv_string "field1"
と
    filed1 = val1
の部分を作る
-}
confBind :: ConfLine -> Q (StmtQ, Q (Name, Exp))
confBind (name, ctype) = do
    n <- newName "x"
    let parser = appE (appE (varE 'val) $ confParser ctype) $ stringE name
    e <- varE n
    return (bindS (varP n) parser, return (mkName name, e))

{-
    val cv_string
の部分をConfTypeごとに作る
-}
confParser :: ConfType -> ExpQ
confParser ConfString = (varE 'cv_string)
confParser ConfURI = (varE 'cv_uri)
confParser (ConfList ctype) = appE (varE 'cv_list) (confParser ctype)

