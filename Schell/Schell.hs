module Schell (
  Expr(Number, String, Symbol, Boolean, List),
  parseSource,
  eval,
  createEnv
) where

import Text.ParserCombinators.Parsec
import Data.Char
import Data.Either
import Text.Printf
import Control.Monad
import Control.Monad.Error
import Data.IORef

data Expr = Number Integer
      | String String 
      | Symbol String
      | Boolean Bool
      | List [Expr]

instance Show Expr where
  show (Number num) = show num
  show (String str) = show str
  show (Symbol sym) = sym
  show (Boolean bool) = if bool then "#t" else "#f"
  show (List exprs) = "(" ++ (joinOn " " $ map show exprs) ++ ")"

instance Eq Expr where
  Number n1 == Number n2 = n1 == n2
  String s1 == String s2 = s1 == s2
  Symbol s1 == Symbol s2 = s1 == s2
  Boolean b1 == Boolean b2 = b1 == b2
  List l1 == List l2 = l1 == l2
  _ == _ = False

data EvalError = BindError String
         | SyntaxError String

instance Error EvalError where
  strMsg s = SyntaxError "error"

instance Show EvalError where
  show (BindError var) = printf "**Error: %s not bound.**" var
  show (SyntaxError msg) = printf "**Error: %s.**" msg

type Env = IORef [(String, IORef Expr)]

createEnv :: IO Env
createEnv = newIORef []

joinOn :: String -> [String] -> String
joinOn _ [x] = x
joinOn joinStr (x:xs) = (x ++ joinStr) ++ (joinOn joinStr xs)

identifierInit :: Parser Char
identifierInit = oneOf "!$%&*/:<=>?~_^" 

readExpr = do
  readLst <|> readString <|> readNumber <|> readSymbol <|> readBoolean 

readSymbol = do
  initial <- letter <|> identifierInit <|> oneOf "+-"
  subsq <- many (letter <|> identifierInit <|> digit <|> oneOf ".+-")
  return $ Symbol (initial:subsq)

readLst = do
  char '('
  x <- sepBy readExpr spaces
  char ')'
  return $ List x

readBoolean = do
  bool <- char '#' >> oneOf "fFtT"
  return . Boolean $ case toLower bool of
               't' -> True
               'f' -> False

negNum = liftM ('-':) (char '-' >> many1 digit)

readNumber = do
  num <- try negNum <|> many1 digit
  return . Number $ (read num :: Integer)

readString = do
  char '"'
  str <- many1 (noneOf "\"")
  char '"'
  return $ String str

parseSource :: String -> Either ParseError Expr 
parseSource input = parse (skipMany space >> readExpr) "No Parse" input


primitives :: [(Expr, [Expr] -> ErrorT EvalError IO Expr)]
primitives = [(Symbol "+", arithmeticOp (+)),
        (Symbol "-", arithmeticOp (-)),
        (Symbol "*", arithmeticOp (*)),
        (Symbol "/", arithmeticOp div),
        (Symbol "<", compOp (<)),
        (Symbol "<=", compOp (<=)),
        (Symbol ">", compOp (>)),
        (Symbol ">=", compOp (>=)),
        (Symbol "=", compOp (==)),
        (Symbol "and", logicOp and),
        (Symbol "or", logicOp or),
        (Symbol "not", notOp),
        (Symbol "cons", cons),
        (Symbol "list", list),
        (Symbol "car", car),
        (Symbol "cdr", cdr)]

isNumberExpr :: Expr -> Bool
isNumberExpr (Number _) = True
isNumberExpr _ = False

isBoolExpr :: Expr -> Bool
isBoolExpr (Boolean _) = True
isBoolExpr _ = False

isListExpr :: Expr -> Bool
isListExpr (List _) = True
isListExpr _ = False

arithmeticOp :: (Integer -> Integer -> Integer) -> [Expr] -> ErrorT EvalError IO Expr
arithmeticOp op exprs 
  | all isNumberExpr exprs = return . Number . foldl1 op $ [x | (Number x) <- exprs]
  | otherwise = throwError $ SyntaxError "**Error: \
      \Arithmetic expressions are of the form: ([+/-/*//] Num1 Num2 .. NumN)**"

compOp :: (Integer -> Integer -> Bool) -> [Expr] -> ErrorT EvalError IO Expr
compOp op [Number a, Number b] = return . Boolean $ op a b
compOp _ _ = throwError $ SyntaxError "**Error: \
  \Comparison expressions are of the form: ([</<=/>/>=/=] Num1 Num2)**"

logicOp :: ([Bool] -> Bool) -> [Expr] -> ErrorT EvalError IO Expr
logicOp op exprs
  | all isBoolExpr exprs = return . Boolean . op $ [b | (Boolean b) <- exprs]
  | otherwise = throwError $ SyntaxError "**Error: \
      \Logical operators are of the form: ([and/or] expr1 expr2 .. exprN)**"

notOp :: [Expr] -> ErrorT EvalError IO Expr
notOp [Boolean b] = return . Boolean . not $ b
notOp _ = throwError $ SyntaxError "**Error: \
 \Not logical operators are of the form: (not expr1)**"

car :: [Expr] -> ErrorT EvalError IO Expr
car [List xs] = return . head $ xs
car _ = throwError $ SyntaxError "**Error: car takes a list of data**"

cdr :: [Expr] -> ErrorT EvalError IO Expr
cdr [List xs] = return . List . tail $ xs
cdr _ = throwError $ SyntaxError "**Error: cdr takes a list of data**"

cons :: [Expr] -> ErrorT EvalError IO Expr
cons [x, List xs] = return . List $ (x:xs)
cons _ = throwError $ SyntaxError "**Error: cons expressions are of the form: (cons x (list ..))**"

list :: [Expr] -> ErrorT EvalError IO Expr
list = return . List 

eval :: Env -> Expr -> ErrorT EvalError IO Expr
eval env (List (Symbol "if":pred:tbr:fbr:[])) = do
  res <- liftIO . runErrorT . eval env $ pred
  case res of
    Left err -> throwError err
    Right (Boolean val)
      | val -> eval env tbr
      | otherwise -> eval env fbr

eval env expr@(List (func:args)) = 
  case lookup func primitives of
    Just f -> mapM (eval env) args >>= apply f
    Nothing -> throwError . BindError . show $ func

eval env expr = return expr
            
apply :: ([Expr] -> ErrorT EvalError IO Expr) -> [Expr] -> ErrorT EvalError IO Expr
apply func args = func args
