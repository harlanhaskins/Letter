module Letter.Parser where

import Prelude hiding (exp)
import Letter.Core
import Control.Monad (void)
import Control.Applicative
import Data.Either
import Text.Megaparsec hiding (space)
import Text.Megaparsec.ByteString
import Text.Megaparsec.Combinator as MPC
import Text.Megaparsec.Prim
import qualified Data.Map as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Lexer as L

type Line = Either (String, FunDef) Exp

infixl <||>
p <||> q = try p <|> try q

choice' :: [Parser a] -> Parser a
choice' = choice . map try

space :: Parser ()
space = L.space (void C.spaceChar) (L.skipLineComment ";") (L.skipBlockComment "#|" "|#")

symbol = L.symbol space

parens = MPC.between (symbol "(") (symbol ")")

lexeme        = L.lexeme space
integer       = lexeme L.integer
signedInteger = L.signed space integer

identifierChar = C.alphaNumChar <||> C.oneOf "*+-/_'=^?!<>"
identifier = some identifierChar
filename = some (identifierChar <||> C.char '.')

line :: Parser Line
line = ((Left <$> funDef) <?> "function definition")
  <||> ((Right <$> exp) <?> "top-level declaration")

exp :: Parser Exp
exp = (nExp <?> "numerical expression")
 <||> (varExp <?> "variable reference")
 <||> parens expBody

expBody :: Parser Exp
expBody = (letExp <?> "let expression")
     <||> (funCall <?> "function call")

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

varExp :: Parser Exp
varExp = Var <$> lexeme identifier

nExp :: Parser Exp
nExp = NExp <$> signedInteger

funDef :: Parser (String, FunDef)
funDef = parens $ do
    symbol "def"
    name <- lexeme identifier
    vars <- parens varList
    e <- exp
    return (name, UserFun vars e)

expList :: Parser [Exp]
expList = (exp `MPC.sepBy` space) <* space

varList :: Parser [String]
varList = (identifier `MPC.sepBy` space) <* space

parseFile :: Parser ([(String, FunDef)], [Exp])
parseFile = do
    lines <- many line <* eof
    return $ partitionEithers lines
