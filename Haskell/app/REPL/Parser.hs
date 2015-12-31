module REPL.Parser where

import Letter.Core
import Letter.Parser
import Control.Monad (void)

data Command = Eval Line
             | Describe Line
             | Import String
             | Quit
             deriving Show

commandToken long short = choice' $ map (symbol . (':':)) [long, short]

importCmd = Import <$> (commandToken "import" "i" >> filename)

describe = commandToken "describe" "d" >> (Describe <$> line)

commandExp = ((\_ -> Quit) <$> commandToken "quit" "q")
        <||> importCmd
        <||> describe
        <||> (Eval <$> line)

command = commandExp <* space
