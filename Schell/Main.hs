module Main where

import System.Exit
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Data.IORef
import Data.Text (unpack, strip, pack)
import System.IO
import System.Environment
import Schell
import Parser

schemeParser = many $ skipMany space >> readExpr

parseSource :: String -> Either ParseError [Expr] 
parseSource input = parse schemeParser "Syntax Error" input

parseFile :: String -> IO (Either ParseError [Expr])
parseFile file = liftM (parseSource . unpack . strip . pack) $ readFile file 

evalAndPrint :: Env -> Either ParseError [Expr] -> IO ()
evalAndPrint env parseResult = 
  case parseResult of
    Left err -> print err
    Right exprs -> 
      (runErrorT . mapM (eval env) $ exprs) >>= either print (mapM_ print)

main :: IO ()
main = do
  args <- getArgs
  env <- createEnv
  emptyEnv <- createEnv
  extendEnv env primitiveSymbols $
    map (\name -> Procedure name emptyEnv [] Void) primitiveSymbols
  if null args 
    then forever $ do
      putStr "schell> " >> hFlush stdout
      input <- getLine
      case words input of
        [":q"] -> exitSuccess
        [":l", file] -> do
          modifyIORef env (\_ -> [])
          parseFile file >>= evalAndPrint env
        sepInput -> evalAndPrint env . parseSource . unwords $ sepInput
    else do
      modifyIORef env (\_ -> [])
      parseFile (head args) >>= evalAndPrint env
