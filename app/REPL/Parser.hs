module REPL.Parser where

import Letter.Core
import Letter.Parser
import Text.Megaparsec
import Control.Monad (void)

data Command = Eval Line
             | Dump Line
             | Quit
             deriving Show

commandTuples = [ ("d", Dump)
                , ("dump", Dump)
                ]

fromCommandTuple (s, f) = symbol (':':s) *> (f <$> line)

commands = choice' $ map fromCommandTuple commandTuples

command = commands
     <||> ((\_ -> Quit) <$> symbol ":q")
     <||> (Eval <$> line)
