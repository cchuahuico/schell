import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import System.Exit
import Data.Char
import qualified Data.Map as Map

data Expr = Number Int 
          | String String 
          | Symbol String
          | Boolean String
          | List [Expr]
    deriving (Show)

identifierInit :: Parser Char
identifierInit = oneOf "!$%&*/:<=>?~_^" 

readExpr = do
    readString <|> readSymbol <|> readBoolean <|> readNumber <|> readLst

readSymbol = do
    initial <- letter <|> identifierInit
    subsq <- many (letter <|> identifierInit <|> digit <|> oneOf ".+-")
    return $ Symbol (initial:subsq)

readLst = do
    char '('
    x <- sepBy readExpr spaces
    char ')'
    return $ List x

readBoolean = do
    bool <- char '#' >> oneOf "fFtT"
    return . Boolean $ '#':[toLower bool] 

readNumber = do
    num <- many1 digit
    return . Number $ (read num :: Int)

readString = do
    char '"'
    str <- many1 (noneOf "\"")
    char '"'
    return $ String str

parseSource :: String -> Either ParseError Expr 
parseSource input = parse readExpr "No Parse" input

main = do
    putStr "schell> "
    input <- getLine
    if input == ":q" then
        exitSuccess
    else
        case parseSource input of 
            (Left _) -> putStrLn "Input Error"
            (Right expr) -> putStrLn . eval $ expr 
    main

eval (Number num) = show num
eval (String str) = show str
eval (Boolean bool) = bool
eval (Symbol sym) = sym
eval (List l) = show l
                        

