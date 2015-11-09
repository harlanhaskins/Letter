module REPL.Core where

import Letter.Core
import Letter.InitialBasis
import Letter.Parser
import Text.Megaparsec
import System.Environment
import System.IO
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

run :: IO ()
run = run' initEnv

handleDef env@(Env fs gs) (Left (id, f)) = do
    let env' = Env (M.insert id f fs) gs
    putStrLn $ id ++ ": " ++ (show f)
    run' env'
handleDef env@(Env fs gs) (Right (Let id exp)) = run' $ Env fs (M.insert id exp gs)
handleDef env (Right exp) = do
    val <- eval env exp
    print val
    run' env

run' :: Env -> IO ()
run' env = getDef "" >>= handleDef env

getDef :: String -> IO (Either (String, FunDef) Exp)
getDef s = do
    x <- putStr (if s == "" then "Letter > " else "         ")
    hFlush stdout
    exp <- getLine
    let newExp = s ++ exp
    case parse line "REPL" (BS.pack newExp) of
        (Left _) -> getDef newExp
        (Right exp) -> return exp
