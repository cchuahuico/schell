module Main where

import System.Exit
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Data.IORef
import System.IO
import Schell

schemeParser = many $ skipMany space >> readExpr

parseSource :: String -> Either ParseError [Expr] 
parseSource input = parse schemeParser "Syntax Error" input

parseFile :: String -> IO (Either ParseError [Expr])
parseFile file = liftM (parseSource . init) $ readFile file 

evalAndPrint :: Env -> Either ParseError [Expr] -> IO ()
evalAndPrint env parseResult = 
  case parseResult of
    Left err -> print err
    Right exprs -> 
      (runErrorT . mapM (eval env) $ exprs) >>= either print (mapM_ print)

main :: IO ()
main = do
  env <- createEnv
  emptyEnv <- createEnv
  extendEnv env primitiveSymbols $
    map (\name -> Procedure name emptyEnv [] Void) primitiveSymbols
  forever $ do
  putStr "schell> " >> hFlush stdout
  input <- getLine
  case words input of
    [":q"] -> exitSuccess
    [":l", file] -> do
      modifyIORef env (\_ -> [])
      parseFile file >>= evalAndPrint env
    sepInput -> evalAndPrint env . parseSource . unwords $ sepInput
