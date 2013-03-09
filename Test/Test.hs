module Main where

import Test.HUnit
import ParseEvalTest

main = runTestTT $ TestList [arithmeticCases]
