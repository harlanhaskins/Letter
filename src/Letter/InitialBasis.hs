{-# LANGUAGE TemplateHaskell #-}

module Letter.InitialBasis where

import Letter.Core
import Letter.Parser
import Text.Megaparsec
import Data.FileEmbed (embedFile)
import Data.Either
import qualified Data.ByteString as BS
import qualified Data.Map as M

binaryFun :: (Int -> Int -> Int) -> FunDef
binaryFun f = BuiltinFun 2 $ \env (e1:e2:_) -> do
    a <- eval env e1
    b <- eval env e2
    return (NExp $ f a b)

checkExpectDef :: Env -> [Exp] -> IO Exp
checkExpectDef env (e1:e2:_) = do
    a <- eval env e1
    b <- eval env e2
    if a == b
    then return $ NExp 0
    else do
        putStrLn $ "check-expect failed. Expected \"" ++ show a ++ "\", got \"" ++ show b ++ "\""
        return $ NExp 0

ifDef :: Env -> [Exp] -> IO Exp
ifDef env (e1:e2:e3:_) = do
    a <- eval env e1
    if a /= 0
    then reduce env e2
    else reduce env e3

printDef :: Env -> [Exp] -> IO Exp
printDef env (e:_) = do
    val <- eval env e
    print val
    return (NExp val)

boolify :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
boolify f = \a b -> if f a b then 1 else 0

builtinDefs :: [(String, FunDef)]
builtinDefs = [ ("+", binaryFun (+))
              , ("*", binaryFun (*))
              , ("-", binaryFun (-))
              , ("/", binaryFun div)
              , ("=", binaryFun (boolify (==)))
              , (">", binaryFun (boolify (>)))
              , ("<", binaryFun (boolify (<)))
              , (">=", binaryFun (boolify (>=)))
              , ("<=", binaryFun (boolify (<=)))
              , ("/=", binaryFun (boolify (/=)))
              , ("mod", binaryFun mod)
              , ("if", BuiltinFun 3 ifDef)
              , ("print", BuiltinFun 1 printDef)
              ]

builtinFuns :: M.Map String FunDef
builtinFuns = M.fromList $
              builtinDefs
           ++ case parsedBuiltins of
                (Left err) -> error $ "Builtins were incorrect. " ++ show err
                (Right (defs, _)) -> defs

initialBasisStr = $(embedFile "src/Letter/initial-basis.ltr")
parsedBuiltins = parse parseFile "" initialBasisStr

emptyEnv = Env mempty mempty
initEnv = Env builtinFuns mempty
