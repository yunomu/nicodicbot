{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
module Config.TH
    ( construct
    , config
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Control.Applicative
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.ByteString (Parser)
import Data.Default
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State

import Config.Parser
import Config.Types
import Config.Lib

config :: QuasiQuoter
config = QuasiQuoter
    { quoteExp = \str -> [|confTmpParser str|]
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

construct :: String -> ConfTmp -> DecsQ
construct name (nameStr, confLines) = f
    <$> mkRecord recName confLines
    <*> instanceDef recName confLines
    <*> mkParsers recName confLines
    <*> mkParser name recName confLines
  where
    recName = mkName nameStr
    f a b c d = a:b:(c++d)

{-
レコードを作る。
-}
mkRecord :: Name -> [ConfLine] -> DecQ
mkRecord recName confLines =
    dataD (cxt []) recName [] [rec] [''Show]
  where
    rec = recC recName $ map confVSType confLines

    confVSType :: ConfLine -> VarStrictTypeQ
    confVSType (name, ctype) = confVSType' name $ confTypeQ ctype

    confVSType' :: String -> TypeQ -> VarStrictTypeQ
    confVSType' name typeq =
        varStrictType (mkName name) $ strictType notStrict typeq

    confTypeQ :: ConfType -> TypeQ
    confTypeQ ConfString = [t|String|]
    confTypeQ ConfURI = [t|String|]
    confTypeQ (ConfList ctype) = [t|[$(confTypeQ ctype)]|]

{-
Data.Defaultのインスタンスにする
-}
instanceDef :: Name -> [ConfLine] -> DecQ
instanceDef recName confLines =
    instanceD (return []) types [func]
  where
    types = [t|Default $(conT recName)|]
    cons = recConE recName $ map defVal confLines
    func = valD (varP 'def) (normalB cons) []

    defVal :: ConfLine -> Q (Name, Exp)
    defVal (n, ConfString) = (,) (mkName n) <$> [|""|]
    defVal (n, ConfURI)    = (,) (mkName n) <$> [|"http://localhost/"|]
    defVal (n, ConfList _) = (,) (mkName n) <$> [|[]|]

mkParsers :: Name -> [ConfLine] -> DecsQ
mkParsers recName confLines =
    concat <$> mapM (mkVal recName) confLines

{-
val_field :: StateT Config Parser ()
val_field = do
    a <- lift $ val cv_string "field"
    c <- get
    put c{field=a}
-}
mkVal :: Name -> ConfLine -> DecsQ
mkVal recName (name, ctype) = do
    s <- sigD funcName sigt
    a <- newName "a"
    c <- newName "c"
    f <- funD funcName [clause [] (body a c) []]
    return [s, f]
  where
    fieldName = mkName name
    funcName = parserName name
    sigt = [t|StateT $(conT recName) Parser ()|]
    body a c = normalB $ doE [
        bindS (varP a) [|lift $ try $ val $(confParser ctype) name|],
        bindS (varP c) [|get|],
        noBindS [|put $(recUpdE (varE c) [(,) fieldName <$> (varE a)])|]]

{-
こういうのを作る
configParser :: Parser Config
configParser = execStateT (lift commentLines *> (many (parsers <* lift commentLines)) <* lift eof) def
-}
mkParser :: String -> Name -> [ConfLine] -> DecsQ
mkParser name recName cl = do
    s <- sigD funcName [t|Parser $(conT recName)|]
    v <- valD (varP funcName) (normalB body) []
    return [s, v]
  where
    funcName = mkName name
    body = [|execStateT (lift commentLines *> (many ($(getFieldParser cl) <* lift commentLines)) <* lift eof) def|]

{-
 - こういう式
fold1 (<|>) [val_string, val_uri, val_int]
 -}
getFieldParser :: [ConfLine] -> ExpQ
getFieldParser confLines = [|foldl1 (<|>) $(listE funcs)|]
  where
    funcs = map (varE . parserName . fst) confLines

parserName :: String -> Name
parserName name = mkName $ "val_" ++ name

{-
    val cv_string
の部分をConfTypeごとに作る
-}
confParser :: ConfType -> ExpQ
confParser ConfString = [|cv_string|]
confParser ConfURI = [|cv_uri|]
confParser (ConfList ctype) = [|cv_list $(confParser ctype)|]

val :: Parser a -> String -> Parser a
val p name = (string name *> spcs *> sep *> p) <* spcs <* commentLine

