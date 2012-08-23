{-# LANGUAGE TemplateHaskell #-}
module Config.TH (construct) where

import Language.Haskell.TH
import Control.Applicative
import Text.Parsec.ByteString (Parser)

import Config.Types
import Config.Lib

construct :: ConfTmp -> DecsQ
construct tmp = (:) <$> mkRecord tmp <*> mkParser tmp

{-
レコードを作る。
-}
mkRecord :: ConfTmp -> DecQ
mkRecord (nameStr, confLines) = do
    let recName = mkName nameStr
    let rec = recC recName $ map confVSType confLines
    dataD (cxt []) recName [] [rec] [''Show]

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
mkParser :: ConfTmp -> DecsQ
mkParser (n, cl) = do
    let recName = mkName n
    let funcName = mkName "configParser"
    (binds, cons) <- unzip <$> mapM confBind cl
    let nob = appE (varE 'return) $ recConE recName cons
    let doe = doE (binds ++ [noBindS nob])
    s <- sigD funcName $ appT (conT ''Parser) (conT recName)
    v <- valD (varP funcName) (normalB doe) []
    return [s, v]

{-
    val1 <- val cv_string "field1"
と
    filed1 = val1
の部分
-}
confBind :: ConfLine -> Q (StmtQ, Q (Name, Exp))
confBind (name, ctype) = do
    bn <- newName "x"
    let s = bindS (varP bn) $ appE (confParser ctype) [|name|]
    e <- varE bn
    return (s, return (mkName name, e))

{-
    val cv_string "field1"
の部分をConfTypeごとに作る
-}
confParser :: ConfType -> ExpQ
confParser ConfString = appE (varE 'val) (varE 'cv_string)
confParser ConfURI = appE (varE 'val) (varE 'cv_uri)
confParser (ConfList ctype) =
    appE (varE 'val) $ appE (varE 'cv_list) (confParser ctype)

