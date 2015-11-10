module REPL.Core where

import Letter.Core
import Letter.InitialBasis
import Letter.Parser
import Text.Megaparsec (parse)
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

handleDef env@(Env fs gs) (Left (id, f)) = do
    let env' = Env (M.insert id f fs) gs
    putStrLn $ id ++ ": " ++ (show f)
    run' env'
handleDef env@(Env fs gs) (Right (Let id exp)) = run' $ Env fs (M.insert id exp gs)
handleDef env@(Env fs gs) (Right exp) = do
    res <- runExceptT $ eval env exp
    case res of
        (Left e) -> do
            putStrLn $ "Letter: " ++ e
            run' env
        (Right val) -> do
            print val
            run' $ Env fs (M.insert "it" (NExp val) gs)

run' :: Env -> IO ()
run' env = getDef "" >>= handleDef env

getDef :: String -> IO (Either (String, FunDef) Exp)
getDef s = do
    x <- putStr (if s == "" then "Letter > " else "         ")
    hFlush stdout
    exp <- tryJust (guard . isEOFError) getLine
    case exp of
        (Left _) -> exitSuccess
        (Right e) -> do
            let newExp = s ++ e
            case parse line "REPL" (BS.pack newExp) of
                (Left _) -> getDef newExp
                (Right exp) -> return exp
