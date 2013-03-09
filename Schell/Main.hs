module Main where

import System.Exit
import Control.Monad
import System.IO
import Schell

main :: IO ()
main = forever $ do
    putStr "schell> " >> hFlush stdout
    input <- getLine
    if input == ":q" 
      then
          exitSuccess
       else
          case parseSource input of 
            (Left err) -> putStrLn $ show err
            (Right expr) -> 
                case eval expr of
                    (Left err) -> putStrLn err
                    (Right res) -> putStrLn . show $ res
