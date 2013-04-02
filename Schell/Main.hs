module Main where

import System.Exit
import Control.Monad
import Control.Monad.Error
import System.IO
import Schell

main :: IO ()
main = do
  env <- createEnv
  emptyEnv <- createEnv
  extendEnv env primitiveSymbols $
    map (\name -> Procedure name emptyEnv [] Void) primitiveSymbols
  forever $ do
  putStr "schell> " >> hFlush stdout
  input <- getLine
  if input == ":q" 
    then
      exitSuccess
     else
       case parseSource input of 
         (Left err) -> print err
         (Right exprs) -> 
           (runErrorT . mapM (eval env) $ exprs) >>= either print (mapM_ print)
