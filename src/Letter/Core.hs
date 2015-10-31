{-# LANGUAGE BangPatterns #-}

module Letter.Core where

import qualified Data.Map as M
import Control.Applicative ((<|>))
import Data.Maybe

data Exp = NExp Int
         | Do [Exp]
         | Var String
         | Let String Exp
         | FunCall String [Exp]
         deriving Show

data Env = Env
         { functions :: M.Map String FunDef
         , globals   :: M.Map String Exp
         } deriving Show

data FunDef = UserFun [String] Exp
            | BuiltinFun Int (Env -> [Exp] -> IO Exp)

instance Show FunDef where
    show (UserFun args e) = "<UserFun:" ++ show (length args) ++ ">"
    show (BuiltinFun a f)   = "<BuiltinFun:" ++ show a ++ ">"

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
    then do
        putStrLn $ "check-expect passed."
        return $ NExp 0
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

builtinFuns = M.fromList
              [ ("+", binaryFun (+))
              , ("*", binaryFun (*))
              , ("-", binaryFun (-))
              , ("/", binaryFun div)
              , ("=", binaryFun (boolify (==)))
              , (">", binaryFun (boolify (>)))
              , ("<", binaryFun (boolify (<)))
              , (">=", binaryFun (boolify (>=)))
              , ("<=", binaryFun (boolify (<=)))
              , ("/=", binaryFun (boolify (/=)))
              , ("not", UserFun ["x"] (FunCall "if" [Var "x", NExp 0, NExp 1]))
              , ("mod", binaryFun mod)
              , ("if", BuiltinFun 3 ifDef)
              , ("print", BuiltinFun 1 printDef)
              , ("check-expect", BuiltinFun 2 checkExpectDef)
              ]

emptyEnv = Env mempty mempty
initEnv = Env builtinFuns mempty

call :: Env -> String -> FunDef -> [Exp] -> IO Exp
call env n (BuiltinFun arity f) args
    | (length args) == arity = f env args
    | otherwise = argsError n arity (length args)
call env@(Env fs gs) n (UserFun ns e) args
    | length args == length ns = do
        vals <- mapM (reduce env) args
        let formals = M.fromList (zip ns vals)
        reduce (Env fs (M.union formals gs)) e
    | otherwise = argsError n (length ns) (length args)

argsError id f a = letterErr $
                   "Invalid number of arguments to function \""
                   ++ id ++ "\". Expected " ++ show f
                   ++ ", got " ++ show a ++ "."

eval :: Env -> Exp -> IO Int
eval env (NExp n) = return n
eval env e        = do
    exp <- reduce env e
    eval env exp

reduce :: Env -> Exp -> IO Exp
reduce !env !n@(NExp _)                         = return n
reduce (Env !fs !gs) (Do ((Let id !e):(!exps))) = reduce (Env fs (M.insert id e gs)) (Do exps)
reduce !env (Do [])                             = return (NExp 0)
reduce !env (Do [!exp])                         = reduce env exp
reduce !env (Do (!exp:(!exps)))                 = do
    _ <- reduce env exp
    reduce env (Do exps)
reduce !env@(Env _ !gs) (Var !id)               = do
    let exp = M.lookup id gs
    case exp of
        Nothing -> letterErr $ "Use of undeclared identifier \"" ++ id ++ "\""
        (Just e) -> reduce env e
reduce !env@(Env fs _) !(FunCall id args)       = do
    let exp = M.lookup id fs
    case exp of
        Nothing -> letterErr $ "Use of undeclared function \"" ++ id ++ "\""
        (Just e) -> call env id e args

letterErr :: String -> a
letterErr = error . ("Error: " ++)
