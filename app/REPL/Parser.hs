module REPL.Parser where

import Letter.Core
import Letter.Parser
import Text.Megaparsec
import Control.Monad (void)

data Command = Eval Line
             | Dump Line
             | Import String
             | Quit
             deriving Show

importCmd = Import <$> ((symbol ":i" <||> symbol ":import") >> filename)

dump = (symbol ":d" >> (Dump <$> line))
  <||> (symbol ":dump" >> (Dump <$> line))

command = ((\_ -> Quit) <$> symbol ":q")
     <||> importCmd
     <||> dump
     <||> (Eval <$> line)
