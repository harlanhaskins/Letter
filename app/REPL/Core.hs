module REPL.Core where

import Letter.Core
import Letter.InitialBasis
import Letter.Parser
import REPL.Parser
import Text.Megaparsec (parse)
import Text.Megaparsec.ByteString
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import Control.Monad.Trans.Except
import Control.Monad
import Control.Exception
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

runRepl :: IO ()
runRepl = run' initEnv

quit = putStrLn "Leaving Letter." >> exitSuccess

handleEval env@(Env fs gs) (Left (id, f)) = do
    let env' = Env (M.insert id f fs) gs
    putStrLn $ id ++ ": " ++ (show f)
    return env'
handleEval env@(Env fs gs) (Right (Let id exp)) = return $ Env fs (M.insert id exp gs)
handleEval env@(Env fs gs) (Right exp) = do
    res <- runExceptT $ eval env exp
    case res of
        (Left e) -> do
            putStrLn $ "Letter: " ++ e
            return env
        (Right val) -> do
            print val
            return $ Env fs (M.insert "it" (NExp val) gs)

showDef :: Show a => String -> a -> String
showDef id e = id ++ " := " ++ show e

handleDump env (Left (id, f)) = do
    putStrLn $ showDef id f
    return env
handleDump env@(Env fs gs) (Right (Var id)) = do
    case M.lookup id fs of
        Nothing -> case M.lookup id gs of
                        Nothing  -> return env
                        Just e -> do
                            putStrLn $ showDef id e
                            return env
        Just e -> do
            putStrLn $ showDef id e
            return env
handleDump env (Right e) = putStrLn (show e) >> return env

handleImport env id = do
    p <- parseFromFile parseFile id
    case p of
        Left _ -> do
            putStrLn $ "Failed to import \"" ++ id ++ "\""
            return env
        Right (funs, _) -> do
            return $ addFuns env funs

handleCmd env (Eval e)    = handleEval env e
handleCmd env (Dump e)    = handleDump env e
handleCmd env (Import id) = handleImport env id
handleCmd env Quit        = quit

run' :: Env -> IO ()
run' env = do
    cmd <- getCmd ""
    env' <- handleCmd env cmd
    run' env'

getCmd :: String -> IO Command
getCmd s = do
    x <- putStr (if s == "" then "Letter> " else "        ")
    hFlush stdout
    exp <- tryJust (guard . isEOFError) getLine
    case exp of
        (Left _) -> putChar '\n' >> quit
        (Right e) -> do
            let newExp = s ++ e
            case parse command "REPL" (BS.pack newExp) of
                (Left _) -> getCmd newExp
                (Right cmd) -> return cmd
