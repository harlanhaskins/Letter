module Main where

import Prelude hiding (exp)
import Letter.Core
import Letter.Parser
import Letter.InitialBasis
import REPL.Core
import qualified Data.Map as M
import Text.Megaparsec.ByteString
import Options.Applicative
import System.Environment

data Config = Config
            { filename :: Maybe String
            , dump     :: Bool
            } deriving Show

config = Config
     <$> (optional . strArgument)
         ( metavar "FILE" )
     <*> switch
         ( long "dump-ast"
        <> short 'd'
        <> help "Dump the AST instead of evaluating" )

fillEnv :: [(String, FunDef)] -> Env -> Env
fillEnv fs (Env fs' gs) = (Env (M.union (M.fromList fs) fs') gs)

evaluate True defs _ = print $ fillEnv defs initEnv
evaluate False defs exps = do
        _ <- eval (fillEnv defs initEnv) (FunCall "do" exps)
        return ()

main :: IO ()
main = do
    (Config filename dump) <- execParser (info config fullDesc)
    case filename of
        (Just fn) -> do
            p <- parseFromFile parseFile fn
            case p of
                (Left err) -> print err
                (Right (defs, exps)) -> evaluate dump defs exps
        Nothing -> run
