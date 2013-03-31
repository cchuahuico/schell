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
  readLst <|> readString <|> readNumber <|> readSymbol <|> readBoolean <|> readCharacter  

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
  bool <- try (char '#' >> oneOf "fFtT")
  return . Boolean $ case toLower bool of
               't' -> True
               'f' -> False

negNum = liftM ('-':) (char '-' >> many1 digit)

readNumber = do
  num <- try negNum <|> many1 digit
  return . Number $ (read num :: Integer)

readCharacter = do
  string "#\\"
  c <- anyChar
  return $ Character c

readString = do
  char '"'
  str <- many1 (noneOf "\"")
  char '"'
  return $ String str

parseSource :: String -> Either ParseError [Expr] 
parseSource input = parse (many $ (skipMany space >> readExpr)) "Syntax Error" input


data Expr = Number Integer
      | Character Char
      | String String 
      | Symbol String
      | Boolean Bool
      | List [Expr]
      | Procedure { procEnv :: Env, procArgs :: [Expr], procBody :: Expr }
      | Void -- mutations using the set! special form has no value so void
             -- is used instead

instance Show Expr where
  show (Number num) = show num
  show (Character char) = "#\\" ++ [char]
  show (String str) = show str
  show (Symbol sym) = sym
  show (Boolean bool) = if bool then "#t" else "#f"
  show (List exprs) = "(" ++ (joinOn " " $ map show exprs) ++ ")"
  show (Procedure _ _ _) = "#<procedure>"
  show Void = "#<void>"

joinOn :: String -> [String] -> String
joinOn _ [x] = x
joinOn joinStr (x:xs) = (x ++ joinStr) ++ (joinOn joinStr xs)

instance Eq Expr where
  Number n1 == Number n2 = n1 == n2
  Character c1 == Character c2 = c1 == c2
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

extendEnv :: Env -> [Expr] -> [Expr] -> IO Env
extendEnv env symbols values = do
  curEnv <- readIORef env
  extendedEnv <- createEnv
  newBindings <- mapM newIORef values >>= return . zip [sym | (Symbol sym) <- symbols] 
  modifyIORef extendedEnv ((newBindings ++ curEnv) ++)
  return extendedEnv

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

isLambda :: Expr -> Bool
isLambda (List (Symbol "lambda":others)) = True
isLambda _ = False

arithmeticOp :: (Integer -> Integer -> Integer) -> [Expr] -> ErrorT EvalError IO Expr
arithmeticOp op exprs 
  | all isNumberExpr exprs = return . Number . foldl1 op $ [x | (Number x) <- exprs]
  | otherwise = throwError $ SyntaxError "**Error: Arithmetic expressions are of the form: ([+/-/*//] Num1 Num2 .. NumN)**"

compOp :: (Integer -> Integer -> Bool) -> [Expr] -> ErrorT EvalError IO Expr
compOp op [Number a, Number b] = return . Boolean $ op a b
compOp _ _ = throwError $ SyntaxError "**Error: Comparison expressions are of the form: ([</<=/>/>=/=] Num1 Num2)**"

logicOp :: ([Bool] -> Bool) -> [Expr] -> ErrorT EvalError IO Expr
logicOp op exprs
  | all isBoolExpr exprs = return . Boolean . op $ [b | (Boolean b) <- exprs]
  | otherwise = throwError $ SyntaxError "**Error: Logical operators are of the form: ([and/or] expr1 expr2 .. exprN)**"

notOp :: [Expr] -> ErrorT EvalError IO Expr
notOp [Boolean b] = return . Boolean . not $ b
notOp _ = throwError $ SyntaxError "**Error: Not logical operators are of the form: (not expr1)**"

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

parseLetBindings :: [Expr] -> ([Expr], [Expr])
parseLetBindings [] = ([], [])
parseLetBindings (x:xs) = let (List [sym, val]) = x in
  ([sym] ++ symNext, [val] ++ valNext)
  where (symNext, valNext) = parseLetBindings xs

eval :: Env -> Expr -> ErrorT EvalError IO Expr
-- eval for special forms
eval env (List [Symbol "if", pred, tbr, fbr]) = do
  res <- eval env pred
  case res of
    (Boolean val)
      | val -> eval env tbr
      | otherwise -> eval env fbr
    _ -> throwError . InvalidArgument $ "1st argument to if must be a predicate"

eval env (List (Symbol "if":_)) = throwError . InvalidArgument $ "syntax error on if special form"

eval env (List [Symbol "define", (List (name:args)), body]) = do
  defineVar env name $ Procedure env args body
  return Void

eval env (List [Symbol "define", ident, expr]) = do
  evaled <- eval env expr
  defineVar env ident evaled 
  return Void

eval env (List (Symbol "define":_)) = throwError . InvalidArgument $ "syntax error on define special form"

eval env (List [Symbol "set!", ident, expr]) = do
  evaled <- eval env expr
  setVar env ident evaled
  return Void

eval env (List (Symbol "set!":_)) = throwError . InvalidArgument $ "syntax error on set! special form"

eval env (List [Symbol "begin"]) = return Void
eval env (List (Symbol "begin":exprs)) = mapM (eval env) exprs >>= return . last

eval env (List [Symbol "lambda", (List args), body]) =
  return . Procedure env args $ body

eval env (List (Symbol "lambda":_)) = throwError . InvalidArgument $ "syntax error on lambda special form"

eval env (List [Symbol "let", (List bindings), body]) =
  let (symbols, vals) = parseLetBindings bindings in
    mapM (eval env) vals >>= applyComplex (Procedure env symbols body) 

eval env (List (Symbol "let":_)) = throwError . InvalidArgument $ "syntax error on let"

-- eval for function application
eval env expr@(List (func:args)) = 
  case lookup func primitives of
    Just f -> mapM (eval env) args >>= applyPrimitive f
    Nothing
      | isLambda func -> do
          proc <- eval env func
          mapM (eval env) args >>= applyComplex proc
      | otherwise -> do
          res <- liftIO. lookupVar env $ func
          case res of
            Just proc -> mapM (eval env) args >>= applyComplex proc
            Nothing -> throwError . UnboundError . show $ func


-- eval for atoms
eval env num@(Number _) = return num
eval env bool@(Boolean _) = return bool
eval env str@(String _) = return str
eval env char@(Character _) = return char

eval env sym = do
  value <- liftIO . lookupVar env $ sym 
  case value of 
    Just expr -> return expr
    Nothing -> throwError . UnboundError . show $ sym
            

-- apply for procedures that are defined in the global environment (ie. primitives)
applyPrimitive :: ([Expr] -> ErrorT EvalError IO Expr) -> [Expr] -> ErrorT EvalError IO Expr
applyPrimitive func args = func args

-- apply for lambdas 
applyComplex :: Expr -> [Expr] -> ErrorT EvalError IO Expr
applyComplex (Procedure env formals body) args 
  | length formals == length args = do
      extended <- liftIO . extendEnv env formals $ args
      eval extended body
  | otherwise = throwError . InvalidArgument $ "arity mismatch in lambda expression"
applyComplex _ _ = throwError . SyntaxError $ "!! Something really bad happened !!"
