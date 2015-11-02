module Letter.Parser where

import Prelude hiding (exp)
import Letter.Core
import Control.Monad (void)
import Control.Applicative
import Data.Either
import Text.Megaparsec (try, (<?>))
import Text.Megaparsec.ByteString
import Text.Megaparsec.Combinator as MPC
import Text.Megaparsec.Prim
import qualified Data.Map as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Lexer as L

space :: Parser ()
space = L.space (void C.spaceChar) (L.skipLineComment ";") (L.skipBlockComment "#|" "|#")

symbol = L.symbol space

parens = MPC.between (symbol "(") (symbol ")")

lexeme        = L.lexeme space
integer       = lexeme L.integer
signedInteger = L.signed space L.integer

identifierChar = (try C.alphaNumChar <|> try (C.oneOf "*+-/_'=^?!<>"))
identifier = some identifierChar

line :: Parser (Either (String, FunDef) Exp)
line = try ((Left <$> funDef) <?> "function definition")
   <|> try ((Right <$> exp) <?> "top-level declaration")

exp :: Parser Exp
exp = try nExp
  <|> try varExp
  <|> try (parens (try expBody))

expBody :: Parser Exp
expBody = try doExp
      <|> try letExp
      <|> try funCall

funCall :: Parser Exp
funCall = do
    name <- lexeme identifier
    args <- expList
    return $ FunCall name args

letExp :: Parser Exp
letExp = do
    symbol "let"
    name <- lexeme identifier
    e <- exp
    return $ Let name e

doExp :: Parser Exp
doExp = Do <$> (symbol "do" *> expList)

varExp :: Parser Exp
varExp = Var <$> lexeme identifier

nExp :: Parser Exp
nExp = NExp . fromIntegral <$> signedInteger

funDef :: Parser (String, FunDef)
funDef = parens $ do
    symbol "def"
    name <- lexeme identifier
    vars <- parens varList
    e <- exp
    return $ (name, UserFun vars e)

expList :: Parser [Exp]
expList = (exp `MPC.sepBy` space) <* space

varList :: Parser [String]
varList = (identifier `MPC.sepBy` space) <* space

parseFile :: Parser ([(String, FunDef)], [Exp])
parseFile = do
    lines <- many line <* eof
    return $ partitionEithers lines
