module Main where

import System.Exit
import Control.Monad
import Control.Monad.Error
import System.IO
import Schell

main :: IO ()
main = do
  env <- createEnv
  forever $ do
  putStr "schell> " >> hFlush stdout
  input <- getLine
  if input == ":q" 
    then
      exitSuccess
     else
       case parseSource input of 
         (Left err) -> putStrLn $ show err
         (Right expr) -> do
           res <- liftIO . runErrorT . eval env $ expr
           case res of
             (Left err) -> putStrLn . show $ err
             (Right res) -> putStrLn . show $ res
