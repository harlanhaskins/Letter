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

main :: IO ()
main = do
    (Config filename dump) <- execParser (info config fullDesc)
    p <- parseFromFile parseFile filename
    if dump then print p
    else do
        case p of
            (Left err) -> print err
            (Right (defs, exps)) -> do
                let env = fillEnv defs initEnv
                _ <- eval env (Do exps)
                return ()
