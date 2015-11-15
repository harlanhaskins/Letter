module Letter.Compiler.Core where

import Letter.Core
import Letter.Parser
import Data.List
import Data.List.Split
import Data.Either
import Data.ByteString.Char8 as B (ByteString, pack, unpack)
import Crypto.Hash
import qualified Data.Map as M

class Compiler a where
    compile :: Compiler a => a -> Line -> String

data CCompiler = CCompiler
instance Compiler CCompiler where
    compile c = either compileFunDef compileExp

compileExp :: Exp -> String
compileExp (NExp n)       = show n
compileExp (Var id)       = id
compileExp (Let id exp)   = "long " ++ id ++ " = " ++ compileExp exp
compileExp (FunCall n es) = compileFun n es

compileFunDef (id, (UserFun args e)) = funDecl id args ++ " {\nreturn " ++ compileExp e ++ ";\n}"
compileFunDef _                = ""

impls :: [(String, FunDef)] -> [Exp] -> [String]
impls funs exps = filter (not . null) $ map ((compile CCompiler) . Left) funs

compileC :: [(String, FunDef)] -> [Exp] -> String
compileC funs exps = intercalate "\n" $
    [ "#include <stdio.h>"
    , intercalate "\n" $ headers ++ bodies
    , "int main() {"
    , compileMany exps
    , "return 0;"
    , "}"
    ]
    where headers = funDeclHeaders funs
          bodies  = impls funs exps

tupleToHeader (id, (UserFun args _)) = funDeclHeader id args

funDeclHeaders :: [(String, FunDef)] -> [String]
funDeclHeaders = map tupleToHeader . filter (isUserFun . snd)

isUserFun :: FunDef -> Bool
isUserFun (UserFun _ _) = True
isUserFun _             = False

funDeclHeader :: String -> [String] -> String
funDeclHeader id args = funDecl id args ++ ";"

funDecl :: String -> [String] -> String
funDecl id args = "long " ++ cleanedFunName id ++ (inParens . intercalate ", " . map ("long " ++)) args

inParens :: String -> String
inParens s = "(" ++ s ++ ")"

infixStandardOps = map pair ["+", "-", "*", "/", "<", ">", "<=", ">="]
    where pair x = (x, x)
infixReplaceOps = [("and", "&&"), ("or", "||"), ("mod", "%"), ("=", "==")]
infixOps :: M.Map String String
infixOps = M.fromList $ infixReplaceOps ++ infixStandardOps

compileMany = intercalate "\n" . map ((++ ";") . compileExp)

compileFun :: String -> [Exp] -> String
compileFun "if" (e1:e2:e3:_) = inParens $ (compileExp e1) ++ " ? " ++ compileExp e2 ++ " : " ++ compileExp e3
compileFun "not" (e:_)       = "!" ++ inParens (compileExp e)
compileFun "print" (e:_)     = "printf(\"%ld\\n\", (long)" ++ compileExp e ++ ")"
compileFun "do" exps = intercalate "\n" $
    [ "({"
    , compileMany exps
    , "});"
    ]
compileFun n exps =
    case (M.lookup n infixOps) of
        Nothing -> (cleanedFunName n) ++ (inParens ((intercalate ", " . map compileExp) exps))
        Just n' -> inParens $ compileExp (head exps) ++ " " ++ n' ++ " " ++ compileExp ((head . tail) exps)

md5 :: B.ByteString -> Digest MD5
md5 = hash

cleanedFunName :: String -> String
cleanedFunName = ("l_" ++) . B.unpack . digestToHexByteString . md5 . B.pack

replace s1 s2 = intercalate s2 . splitOn s1
