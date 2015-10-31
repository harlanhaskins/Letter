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
            | BuiltinFun (Env -> [Exp] -> IO Exp)

instance Show FunDef where
    show (UserFun args e) = "UserFun " ++ show args ++ " " ++ show e
    show (BuiltinFun f)   = "<BuiltinFun>"

binaryFun :: (Int -> Int -> Int) -> FunDef
binaryFun f = BuiltinFun $ \env (e1:e2:_) -> do
    a <- eval env e1
    b <- eval env e2
    return (NExp $ f a b)

ifDef :: (Env -> [Exp] -> IO Exp)
ifDef env (e1:e2:e3:_) = do
    a <- eval env e1
    if a /= 0
    then reduce env e2
    else reduce env e3

printDef :: (Env -> [Exp] -> IO Exp)
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
              , ("mod", binaryFun mod)
              , ("if", BuiltinFun ifDef)
              , ("print", BuiltinFun printDef)
              ]

emptyEnv = Env mempty mempty
initEnv = Env builtinFuns mempty

call env (BuiltinFun f) args = f env args
call env@(Env fs gs) (UserFun ns e) args = do
    vals <- mapM (reduce env) args
    let formals = M.fromList (zip ns vals)
    reduce (Env fs (M.union formals gs)) e

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
        (Just e) -> call env e args

letterErr = error . ("Error: " ++)
