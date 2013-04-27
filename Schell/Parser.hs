module Parser where 

import Text.ParserCombinators.Parsec
import Data.Char
import Control.Monad
import Schell

-- list of allowed initial characters for scheme identifiers
identifierInit :: Parser Char
identifierInit = oneOf "!$%&*/:<=>?~_^" 

readExpr :: Parser Expr
readExpr = 
  readLst <|> readString <|> readNumber <|> readSymbol <|> readBoolean <|> readCharacter  

readSymbol :: Parser Expr
readSymbol = do
  initial <- letter <|> identifierInit <|> oneOf "+-"
  subsq <- many (letter <|> identifierInit <|> digit <|> oneOf ".+-")
  return $ Symbol (initial:subsq)

readLst :: Parser Expr
readLst = do
  char '('
  x <- sepBy readExpr spaces
  char ')'
  return $ List x

readBoolean :: Parser Expr
readBoolean = do
  bool <- try (char '#' >> oneOf "fFtT")
  return . Boolean $ case toLower bool of
               't' -> True
               'f' -> False

negNum :: Parser String
negNum = liftM ('-':) (char '-' >> many1 digit)

readNumber :: Parser Expr
readNumber = do
  num <- try negNum <|> many1 digit
  return . Number $ (read num :: Integer)

readCharacter :: Parser Expr
readCharacter = do
  string "#\\"
  c <- anyChar
  return $ Character c

readString :: Parser Expr
readString = do
  char '"'
  str <- many1 (noneOf "\"")
  char '"'
  return $ String str
