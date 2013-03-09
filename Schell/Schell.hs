module Schell.Schell (
    Expr,
    parseSource,
    eval
) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import System.Exit
import Data.Char
import Data.Either
import Text.Printf

data Expr = Number Int 
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

identifierInit :: Parser Char
identifierInit = oneOf "!$%&*/:<=>?~_^" 

readExpr = do
    readString <|> readSymbol <|> readBoolean <|> readNumber <|> readLst

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

readNumber = do
    num <- many1 digit
    return . Number $ (read num :: Int)

readString = do
    char '"'
    str <- many1 (noneOf "\"")
    char '"'
    return $ String str

parseSource :: String -> Either ParseError Expr 
parseSource input = parse (skipMany space >> readExpr) "No Parse" input

main :: IO ()
main = do
    putStr "schell> "
    input <- getLine
    if input == ":q" then
        exitSuccess
    else
        case parseSource input of 
            (Left _) -> putStrLn "Input Error"
            (Right expr) -> 
                case eval expr of
                    (Left err) -> putStrLn err
                    (Right res) -> putStrLn . show $ res
    main


primitives :: [(Expr, [Expr] -> Either String Expr)]
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

arithmeticOp :: (Int -> Int -> Int) -> [Expr] -> Either String Expr
arithmeticOp op exprs 
    | all isNumberExpr exprs = Right . Number $ foldl1 op [x | (Number x) <- exprs]
    | otherwise = Left "**Error: \
          \Arithmetic expressions are of the form: ([+/-/*//] Num1 Num2 .. NumN)**"

compOp :: (Int -> Int -> Bool) -> [Expr] -> Either String Expr
compOp op [Number a, Number b] = Right . Boolean $ op a b
compOp _ _ = Left "**Error: \
    \Comparison expressions are of the form: ([</<=/>/>=/=] Num1 Num2)**"

logicOp :: ([Bool] -> Bool) -> [Expr] -> Either String Expr
logicOp op exprs
    | all isBoolExpr exprs = Right . Boolean . op $ [b | (Boolean b) <- exprs]
    | otherwise = Left "**error: \
          \Logical operators are of the form: ([and/or] expr1 expr2 .. exprN)**"

notOp :: [Expr] -> Either String Expr
notOp [Boolean b] = Right . Boolean . not $ b
notOp _ = Left "**Error: \
    \Not logical operators are of the form: (not expr1)**"

car :: [Expr] -> Either String Expr
car [List xs] = Right . head $ xs
car _ = Left "**Error: car takes a list of data**"


cdr :: [Expr] -> Either String Expr
cdr [List xs] = Right . List . tail $ xs
cdr _ = Left "**Error: cdr takes a list of data**"

cons :: [Expr] -> Either String Expr
cons [x, List xs] = Right . List $ (x:xs)
cons _ = Left "**Error: cons expressions are of the form: (cons x (list ..))**"

list :: [Expr] -> Either String Expr
list = Right . List 

ifConditional :: [Expr] -> Either String Expr
ifConditional [Boolean pred, tbr, fbr] 
  | pred = eval tbr
  | otherwise = eval fbr

eval :: Expr -> Either String Expr
eval (List (Symbol "if":pred:args)) =
    case eval pred of
        Left _ -> Left "**Error: malformed if expression**"
        Right boolVal -> ifConditional (boolVal:args)

eval expr@(List (func:args)) = 
    case lookup func primitives of
        (Just f) 
            | null . lefts $ evalArgs -> apply f $ rights evalArgs
            | otherwise -> Left . head . lefts $ evalArgs
        Nothing -> Left (printf "**Error: function \"%s\" is unbound**" 
                         (show func) :: String)
    where evalArgs = map eval args

eval expr = Right expr
                        
apply :: ([Expr] -> Either String Expr) -> [Expr] -> Either String Expr
apply func args = func args
