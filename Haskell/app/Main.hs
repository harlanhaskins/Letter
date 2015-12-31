module Main where

import Prelude hiding (exp)
import Letter.Core
import Letter.Parser
import Letter.Analyzer
import Letter.Optimizer
import Letter.InitialBasis
import Letter.Compiler.Core
import REPL.Core
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Arrow (second)
import qualified Data.Map as M
import Data.List (intercalate)
import Text.Megaparsec.ByteString
import Options.Applicative
import System.Environment

data Config = Config
            { filename          :: Maybe String
            , dump              :: Bool
            , target            :: Maybe String
            , optimizationLevel :: Int
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
     <*> option auto
         ( long "optimization"
        <> short 'O'
        <> value 0
        <> help "How many optimization passes to apply." )

fillEnv :: [(String, FunDef)] -> Env -> Env
fillEnv fs (Env fs' gs) = Env (M.union (M.fromList fs) fs') gs

evaluate :: Bool -> Env -> [Exp] -> IO ()
evaluate True env exps = (putStrLn . intercalate "\n")
    [ dumpEnv env
    , intercalate "\n" $ map show exps
    ]
evaluate False env exps = do
    res <- runExceptT $ eval env (FunCall "do" exps)
    case res of
        (Left e) -> putStrLn $ "Letter: " ++ e
        _        -> return ()


optimizeFuns n defs = iterate (map (second optimizeFun)) defs !! n
optimizeExps n exps = iterate (map optimizeExp) exps !! n

dumpEnv :: Env -> String
dumpEnv (Env fs gs) = dumpDefs fs

dumpDefs :: M.Map String FunDef -> String
dumpDefs = intercalate "\n" . map dumpDef . M.toList
    where dumpDef (id, f) = id ++ " := " ++ show f

handleFile fn dump target optim = do
    p <- parseFromFile parseFile fn
    case p of
        (Left err) -> print err
        (Right ls@(defs, exps)) -> do
            let defs' = optimizeFuns optim defs
            let exps' = optimizeExps optim exps
            case target of
                Nothing -> do
                    let env = fillEnv defs' initEnv
                    case (exps' >>= analyzeExp env) ++ (defs' >>= analyzeFun env) of
                        [] -> evaluate dump env exps'
                        xs -> (putStrLn . intercalate "\n") xs
                Just t  -> putStrLn $ compileC (builtinFunDefs ++ defs') exps'

main :: IO ()
main = do
    c@(Config filename dump target optim) <- execParser (info config fullDesc)
    case filename of
        (Just fn) -> handleFile fn dump target optim
        Nothing -> runRepl
