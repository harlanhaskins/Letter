{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns    #-}

module Letter.InitialBasis where

import Letter.Core
import Letter.Parser
import Text.Megaparsec (parse)
import Data.FileEmbed (embedFile)
import Data.Either
import Control.Error
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.Map as M

binaryFun :: (Integer -> Integer -> Integer) -> FunDef
binaryFun f = BuiltinFun (Just 2) $ \env (e1:e2:_) -> do
    a <- eval env e1
    b <- eval env e2
    return (NExp (f a b))

divDef env (e1:e2:_) = do
    a <- eval env e1
    b <- eval env e2
    if b == 0
    then throwE "Cannot divide by zero."
    else return (NExp (div a b))

checkExpectDef :: Env -> [Exp] -> LetterResult Exp
checkExpectDef env (e1:e2:_) = do
    a <- eval env e1
    b <- eval env e2
    if a == b
    then return $ NExp 0
    else throwE $ "check-expect failed. Expected \"" ++ show a ++ "\", got \"" ++ show b ++ "\""

doDef :: Env -> [Exp] -> LetterResult Exp
doDef env []   = return $ NExp 0
doDef env@(Env fs gs) ((Let !id !e):(!es)) = doDef (Env fs (M.insert id e gs)) es
doDef env [!e] = reduce env e
doDef env (!e:(!es)) = do
    _ <- reduce env e
    doDef env es

ifDef :: Env -> [Exp] -> LetterResult Exp
ifDef env (e1:e2:e3:_) = do
    a <- eval env e1
    if a /= 0
    then reduce env e2
    else reduce env e3

printDef :: Env -> [Exp] -> LetterResult Exp
printDef env (e:_) = do
    res <- lift . runExceptT $ eval env e
    case res of
        (Right val) -> (lift $ print val) >> return (NExp val)
        (Left e)    -> throwE e

toBool 0 = False
toBool _ = True

boolify :: (Integer -> Integer -> Bool) -> (Integer -> Integer -> Integer)
boolify f = \a b -> if f a b then 1 else 0

orDef :: Env -> [Exp] -> LetterResult Exp
orDef env (e1:e2:_) = do
    a <- eval env e1
    if a /= 0
    then return (NExp a)
    else reduce env e2

andDef :: Env -> [Exp] -> LetterResult Exp
andDef env (e1:e2:_) = do
    a <- eval env e1
    if a == 0
    then return (NExp a)
    else reduce env e2

builtinDefs :: [(String, FunDef)]
builtinDefs = [ ("+", binaryFun (+))
              , ("*", binaryFun (*))
              , ("-", binaryFun (-))
              , ("=", binaryFun (boolify (==)))
              , (">", binaryFun (boolify (>)))
              , ("<", binaryFun (boolify (<)))
              , (">=", binaryFun (boolify (>=)))
              , ("<=", binaryFun (boolify (<=)))
              , ("/=", binaryFun (boolify (/=)))
              , ("/", BuiltinFun (Just 2) divDef)
              , ("or", BuiltinFun (Just 2) orDef)
              , ("and", BuiltinFun (Just 2) andDef)
              , ("mod", binaryFun mod)
              , ("if", BuiltinFun (Just 3) ifDef)
              , ("print", BuiltinFun (Just 1) printDef)
              , ("do", BuiltinFun Nothing doDef)
              ]

builtinFuns = M.fromList builtinFunDefs

builtinFunDefs :: [(String, FunDef)]
builtinFunDefs = builtinDefs
           ++ case parsedBuiltins of
                (Left err) -> error $ "Builtins were incorrect. " ++ show err
                (Right (defs, _)) -> defs

initialBasisStr = $(embedFile "src/Letter/initial-basis.ltr")
parsedBuiltins = parse parseFile "" initialBasisStr

emptyEnv = Env mempty mempty
initEnv = Env builtinFuns mempty
