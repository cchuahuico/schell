import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import System.Exit
import Data.Char

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
            (Right expr) -> putStrLn . show . eval $ expr 
    main


primitives :: [(Expr, [Expr] -> Expr)]
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

arithmeticOp :: (Int -> Int -> Int) -> [Expr] -> Expr
arithmeticOp op exprs = Number $ foldl1 op $ [x | (Number x) <- exprs]

compOp :: (Int -> Int -> Bool) -> [Expr] -> Expr
compOp op args@[Number a, Number b] = Boolean $ op a b
compOp _ _ = error "Invalid arguments for comparison operator."

logicOp :: ([Bool] -> Bool) -> [Expr] -> Expr
logicOp op exprs = Boolean $ op [b | (Boolean b) <- exprs]

notOp :: [Expr] -> Expr
notOp [Boolean b] = Boolean $ not b

car :: [Expr] -> Expr
car [List xs] = head xs

cdr :: [Expr] -> Expr
cdr [List xs] = List $ tail xs

cons :: [Expr] -> Expr
cons [x, List xs] = List (x:xs)

list :: [Expr] -> Expr
list = List 

ifConditional :: [Expr] -> Expr
ifConditional [Boolean pred, tbr, fbr] 
  | pred = eval tbr
  | otherwise = eval fbr

eval :: Expr -> Expr
eval (List (Symbol "if":pred:args)) = ifConditional (eval pred:args :: [Expr])
eval expr@(List (func:args)) = 
    case lookup func primitives of
        (Just f) -> apply f $ map eval args
        Nothing -> expr
eval expr = expr
                        
apply :: ([Expr] -> Expr) -> [Expr] -> Expr
apply func args = func args
