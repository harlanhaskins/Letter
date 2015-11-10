{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}

module Letter.Core where

import qualified Data.Map as M
import Control.Applicative ((<|>))
import Data.Maybe
import Control.Error
import Control.Monad.IO.Class
import Control.Monad.Trans

type LetterResult = ExceptT String IO

addGlobals (Env fs gs) vars = Env fs (M.union (M.fromList vars) gs)
addFun     (Env fs gs) id f = Env (M.insert id f fs) gs
addGlobal  (Env fs gs) id e = Env fs (M.insert id e gs)
findGlobal (Env _ gs) id    = M.lookup id gs
findFun    (Env fs _) id    = M.lookup id fs

data Exp = NExp !Int
         | Var !String
         | Let !String !Exp
         | FunCall !String ![Exp]
         deriving Show

data Env = Env
         { functions :: M.Map String FunDef
         , globals   :: M.Map String Exp
         } deriving Show

data FunDef = UserFun ![String] !Exp
            | BuiltinFun (Maybe Int) (Env -> [Exp] -> LetterResult Exp)

instance Show FunDef where
    show (UserFun args e) = "<UserFun:" ++ show (length args) ++ ">"
    show (BuiltinFun a f)   = "<BuiltinFun:" ++ showArity a ++ ">"
        where showArity Nothing = "∞"
              showArity (Just a) = show a

call :: Env -> String -> FunDef -> [Exp] -> LetterResult Exp
call env n (BuiltinFun Nothing f) !args = f env args
call env n (BuiltinFun (Just arity) f) !args
    | (length args) == arity = f env args
    | otherwise = argsError n arity (length args)
call env n (UserFun !ns !e) !args
    | length args == length ns = do
        vals <- mapM (reduce env) args
        let formals = zip ns vals
        reduce (addGlobals env formals) e
    | otherwise = argsError n (length ns) (length args)

argsError :: String -> Int -> Int -> LetterResult Exp
argsError id f a = throwE $
                   "Invalid number of arguments to function \""
                   ++ id ++ "\". Expected " ++ show f
                   ++ ", got " ++ show a ++ "."

eval :: Env -> Exp -> LetterResult Int
eval _   (NExp n) = return n
eval env e        = reduce env e >>= eval env

reduce :: Env -> Exp -> LetterResult Exp
reduce env n@(NExp _) = liftIO $ return n
reduce env (Var !id)   = do
    case findGlobal env id of
        Nothing -> throwE $ "Use of undeclared identifier \"" ++ id ++ "\""
        (Just e) -> reduce env e
reduce env (FunCall !id !args) = do
    case findFun env id of
        Nothing -> throwE $ "Use of undeclared function \"" ++ id ++ "\""
        (Just f) -> call env id f args
