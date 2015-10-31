module Main where

import Prelude hiding (exp)
import Letter.Core
import Letter.Parser
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

main :: IO ()
main = do
    (Config filename dump) <- execParser (info config fullDesc)
    p <- parseFromFile parseFile filename
    if dump then print p
    else do
        case p of
            (Left err) -> print err
            (Right (env, exps)) -> do
                _ <- eval env (Do exps)
                return ()
