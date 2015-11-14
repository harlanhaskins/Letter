module Main where

import Prelude hiding (exp)
import Letter.Core
import Letter.Parser
import Letter.InitialBasis
import Letter.Compiler.Core
import REPL.Core
import Control.Monad.Trans
import Control.Monad.Trans.Except
import qualified Data.Map as M
import Data.List (intercalate)
import Text.Megaparsec.ByteString
import Options.Applicative
import System.Environment

data Config = Config
            { filename :: Maybe String
            , dump     :: Bool
            , target   :: Maybe String
            } deriving Show

config = Config
     <$> (optional . strArgument)
         ( metavar "FILE" )
     <*> switch
         ( long "dump-ast"
        <> short 'd'
        <> help "Dump the AST instead of evaluating" )
     <*> (optional . strOption)
         ( long "target"
        <> short 't'
        <> help "The target for compilation" )

fillEnv :: [(String, FunDef)] -> Env -> Env
fillEnv fs (Env fs' gs) = (Env (M.union (M.fromList fs) fs') gs)

evaluate :: Bool -> [(String, FunDef)] -> [Exp] -> IO ()
evaluate True defs _ = (putStrLn . dumpEnv . fillEnv defs) initEnv
evaluate False defs exps = do
    res <- runExceptT $ eval (fillEnv defs initEnv) (FunCall "do" exps)
    case res of
        (Left e) -> putStrLn $ "Letter: " ++ e
        _        -> return ()

dumpEnv :: Env -> String
dumpEnv (Env fs gs) = dumpDefs fs

dumpDefs :: M.Map String FunDef -> String
dumpDefs = intercalate "\n" . map dumpDef . M.toList
    where dumpDef (id, f) = id ++ " := " ++ show f

main :: IO ()
main = do
    (Config filename dump target) <- execParser (info config fullDesc)
    case filename of
        (Just fn) -> do
            p <- parseFromFile parseFile fn
            case p of
                (Left err) -> print err
                (Right ls@(defs, exps)) -> case target of
                    Nothing -> evaluate dump defs exps
                    Just t  -> putStrLn $ compileC (builtinFunDefs ++ defs) exps
        Nothing -> runRepl
