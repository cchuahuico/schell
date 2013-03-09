module ParseEvalTest where 

import Test.HUnit
import Schell

parseEval :: String -> Expr
parseEval src = case parseSource src of 
                  (Left err) -> String "Parse error"
                  (Right expr) -> 
                      case eval expr of 
                        (Left err) -> String err
                        (Right expr) -> expr

arithmeticCases = TestLabel "Test Arithmetic Procedures" 
    (TestList [testPlus, testMinus, testMultiply, testDivide])

testPlus = TestCase $ do
    assertEqual "plus-11" (Number 11) (parseEval "(+ 1 2 5 3)")
    assertEqual "plus-neg" (Number (-5)) (parseEval "(+ 20 -25)")

testMinus = TestCase $ do
    assertEqual "minus-7" (Number 7) (parseEval "(- 10 3)")
    assertEqual "minus-neg" (Number (-1)) (parseEval "(- 4 0 1 4)")

testMultiply = TestCase $ do
    assertEqual "mul-56" (Number 56) (parseEval "(* 8 7)")
    assertEqual "mul-neg" (Number (-5)) (parseEval "(* 5 -1 -1 -1)")

testDivide = TestCase $ do
    assertEqual "div-0" (Number 0) (parseEval "(/ 0 17 3)")
    assertEqual "div-10" (Number 10) (parseEval "(/ 100 10)")
