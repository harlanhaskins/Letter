module Main where

import Prelude hiding (exp)
import Letter.Core
import Letter.Parser
import Letter.InitialBasis
import qualified Data.Map as M
import Text.Megaparsec.ByteString
import Options.Applicative
import System.Environment

data Config = Config
            { filename :: String
            , dump     :: Bool
            } deriving Show

config = Config
     <$> argument str
         ( metavar "FILE" )
     <*> switch
         ( long "dump-ast"
        <> short 'd'
        <> help "Dump the AST instead of evaluating" )

fillEnv :: [(String, FunDef)] -> Env -> Env
fillEnv fs (Env fs' gs) = (Env (M.union (M.fromList fs) fs') gs)

run True defs _ = print $ fillEnv defs initEnv
run False defs exps = do
        _ <- eval (fillEnv defs initEnv) (FunCall "do" exps)
        return ()

main :: IO ()
main = do
    (Config filename dump) <- execParser (info config fullDesc)
    p <- parseFromFile parseFile filename
    case p of
        (Left err) -> print err
        (Right (defs, exps)) -> run dump defs exps
