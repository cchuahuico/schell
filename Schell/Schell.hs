module Schell where

import Text.ParserCombinators.Parsec
import Data.Char
import Data.Either
import Text.Printf
import Control.Monad
import Control.Monad.Error
import Data.IORef


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
parseSource input = parse (skipMany space >> readExpr) "Syntax Error" input



data Expr = Number Integer
      | String String 
      | Symbol String
      | Boolean Bool
      | List [Expr]
      | Void -- mutations using the set! special form has no value so void
             -- is used instead

instance Show Expr where
  show (Number num) = show num
  show (String str) = show str
  show (Symbol sym) = sym
  show (Boolean bool) = if bool then "#t" else "#f"
  show (List exprs) = "(" ++ (joinOn " " $ map show exprs) ++ ")"
  show Void = "<#void>"

joinOn :: String -> [String] -> String
joinOn _ [x] = x
joinOn joinStr (x:xs) = (x ++ joinStr) ++ (joinOn joinStr xs)

instance Eq Expr where
  Number n1 == Number n2 = n1 == n2
  String s1 == String s2 = s1 == s2
  Symbol s1 == Symbol s2 = s1 == s2
  Boolean b1 == Boolean b2 = b1 == b2
  List l1 == List l2 = l1 == l2
  _ == _ = False



data EvalError = UnboundError String
         | SyntaxError String
         | InvalidArgument String
         | BindExists String

instance Error EvalError where
  strMsg s = SyntaxError "error"

instance Show EvalError where
  show (UnboundError var) = printf "**Error: identifier: %s not bound.**" var
  show (SyntaxError msg) = printf "**Error: %s.**" msg
  show (InvalidArgument msg) = printf "**Error: %s. **" msg
  show (BindExists var) = printf "**Error: binding for %s already exists. **" var



-- the type that represents the global environment
-- IORefs are used since scheme allows mutations in its environment
type Env = IORef [(String, IORef Expr)]

createEnv :: IO Env
createEnv = newIORef []

defineVar :: Env -> Expr -> Expr -> ErrorT EvalError IO ()
defineVar env symb@(Symbol sym) expr = do
  val <- liftIO . lookupVar env $ symb
  case val of 
    Just actual -> throwError . BindExists $ sym
    Nothing -> liftIO $ do {actual <- newIORef $ expr; 
      modifyIORef env ((sym, actual):)}

setVar :: Env -> Expr -> Expr -> ErrorT EvalError IO ()
setVar env symb@(Symbol sym) newVal = do
  envClose <- liftIO . readIORef $ env
  case lookup sym envClose of 
    Just val -> liftIO . modifyIORef val $ (\_ -> newVal)  
    Nothing -> throwError . UnboundError $ sym

lookupVar :: Env -> Expr -> IO (Maybe Expr)
lookupVar env (Symbol sym) = do
  envClose <- readIORef env
  case lookup sym envClose of
    Just val -> readIORef val >>= (\actual -> return . Just $ actual)
    Nothing -> return Nothing

-- primitive functions that's referenced by apply
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
-- eval for special forms
eval env (List (Symbol "if":pred:tbr:fbr:[])) = do
  res <- eval env pred
  case res of
    (Boolean val)
      | val -> eval env tbr
      | otherwise -> eval env fbr
    _ -> throwError . InvalidArgument $ "1st argument to if must be a predicate"

eval env (List (Symbol "if":_)) = throwError . InvalidArgument $ "syntax error on if special form"

eval env (List (Symbol "define":ident:expr:[])) = do
  evaled <- eval env expr
  defineVar env ident evaled 
  return Void

eval env (List (Symbol "define":_)) = throwError . InvalidArgument $ "syntax error on define special form"

eval env (List (Symbol "set!":ident:expr:[])) = do
  evaled <- eval env expr
  setVar env ident evaled
  return Void

eval env (List (Symbol "set!":_)) = throwError . InvalidArgument $ "syntax error on set! special form"

-- eval for function application
eval env expr@(List (func:args)) = 
  case lookup func primitives of
    Just f -> mapM (eval env) args >>= apply f
    Nothing -> throwError . UnboundError . show $ func

-- eval for atoms
eval env num@(Number _) = return num
eval env bool@(Boolean _) = return bool
eval env str@(String _) = return str

eval env sym = do
  value <- liftIO . lookupVar env $ sym 
  case value of 
    Just expr -> return expr
    Nothing -> throwError . UnboundError . show $ sym
            

-- the apply function is limited to primitives for now, until lambdas are implemented
apply :: ([Expr] -> ErrorT EvalError IO Expr) -> [Expr] -> ErrorT EvalError IO Expr
apply func args = func args
