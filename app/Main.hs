module Main where

import Prelude hiding (exp)
import Letter.Core
import Letter.Parser
import Text.Megaparsec.ByteString
import System.Environment

main :: IO ()
main = do
    [filename] <- getArgs
    p <- parseFromFile parseFile filename
    print p
    case p of
        (Left err) -> print err
        (Right (env, exps)) -> do
            val <- eval env (Do exps)
            print val
