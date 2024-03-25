{-|
Module: A2StarterTests
Description: Starter Tests for A2
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2023
-}

module A2StarterTest

where

import Test.QuickCheck (Property, (==>), label, quickCheck)

import A2 (run, eval)
import A2Types(Expr(..), Value(..), Env)

--Some simple tests to get you started--

prop_testLiteralNumber:: Int -> Property
prop_testLiteralNumber x = label "literal numbers" $
    let expr = (Literal $ Num x)
        result = run expr
    in result == Num x

prop_testLiteralError:: String -> Property
prop_testLiteralError x = label "literal errors" $
    let expr = (Literal $ Error x)
        result = run expr
    in result == Error "Literal"

prop_testAddition:: Int -> Int -> Property
prop_testAddition x y = label "addition tests" $
    let expr = (Plus (Literal $ Num x) (Literal $ Num y))
        result = run expr
    in result == Num (x + y)

prop_testAddition2:: Property
prop_testAddition2 = label "addition test 2" $
    let expr = (Plus (Literal $ Num 3) (Times (Literal $ Num 3) (Literal $ Num 7)))
        result = run expr
    in result == Num 24

prop_testBadAddition:: Int -> Int -> Property
prop_testBadAddition x y = label "addition tests error" $
    let expr = (Plus (Literal T) (Literal $ Num y))
        result = run expr
    in result == Error "Plus"

prop_testBasicIdentifier :: Property
prop_testBasicIdentifier = label "identifier error" $
  let expr = (Plus (Literal $ Num 3) (Times (Literal $ Num 3) (Literal T)))
      result = run expr
  in result == Error "Times"

prop_testEqual1 :: Property
prop_testEqual1 = label "equal test" $
    let expr = (Equal (Plus (Literal $ Num 3) (Literal $ Num 4)) (Literal $ Num 7))
        result = run expr
    in result == T

prop_testEqual2 :: Property
prop_testEqual2 = label "equal test 2" $
    let expr = (Equal (Literal $ Num 3) (Literal $ Num 4))
        result = run expr
    in result == F

prop_testIf1 :: Property
prop_testIf1 = label "if test" $
    let expr = (If (Literal T) (Literal $ Num 3) (Plus (Literal T) (Literal F)))
        result = run expr
    in result == Num 3

prop_testIf2 :: Property
prop_testIf2 = label "if test 2" $
    let expr = (If (Literal F) (Literal $ Num 3) (Plus (Literal T) (Literal F)))
        result = run expr
    in result == Error "Plus"

prop_testCons1 :: Property
prop_testCons1 = label "cons test" $
    let expr = (Cons (Literal $ T) (Literal $ F))
        result = run expr
    in result == Pair T F

prop_testCons2 :: Property
prop_testCons2 = label "cons test 2" $
    let expr = (Cons (If (Literal F) (Literal $ Num 3) (Plus (Literal T) (Literal F))) (Literal $ F))
        result = run expr
    in result == Error "Plus"

prop_testFirst1 :: Property
prop_testFirst1 = label "first test" $
    let expr = (First (Cons (Literal $ T) (Literal $ F)))
        result = run expr
    in result == T

prop_testFirst2 :: Property
prop_testFirst2 = label "first test 2" $
    let expr = (First (Cons (Equal (Literal $ Num 3) (Literal $ Num 4)) (Literal $ F)))
        result = run expr
    in result == F

prop_testFirst3 :: Property
prop_testFirst3 = label "first test 3" $
    let expr = (First (Cons (If (Literal F) (Literal $ Num 3) (Plus (Literal T) (Literal F))) (Literal $ F)))
        result = run expr
    in result == Error "First"

prop_testFirst4 :: Property
prop_testFirst4 = label "first test 4" $
    let expr = (First (Literal $ F))
        result = run expr
    in result == Error "First"

prop_testRest1 :: Property
prop_testRest1 = label "rest test" $
    let expr = (Rest (Cons (Literal $ T) (Literal $ F)))
        result = run expr
    in result == F

prop_testRest2 :: Property
prop_testRest2 = label "rest test 2" $
    let expr = (Rest (Literal $ F))
        result = run expr
    in result == Error "Rest"

prop_testFunctionApplication :: Int -> Int -> Property
prop_testFunctionApplication x y = label "function application" $
    let fnExpr1 = Lambda ["x"] (Plus (Literal (Num 1)) (Var "x"))
        fnExpr2 = Lambda ["a", "b"] (Times (Var "a") (App fnExpr1 [(Var "b")]))
        result = run (App fnExpr2 [(Literal (Num x)), (Literal (Num y))])
    in result == Num (x * (y + 1))

prop_testMultiplication:: Int -> Int -> Property
prop_testMultiplication x y = label "multiplication tests" $
    let expr = Times (Literal $ Num x) (Literal $ Num y)
        result = run expr
    in result == Num (x * y)


-------------------------------------------------------------------------------
-- * Main function (for testing purposes only)
-------------------------------------------------------------------------------

-- This main function is executed when you compile and run this Haskell file.
-- It runs the QuickCheck tests; we'll talk about "do" notation much later in
-- the course, but for now if you want to add your own tests, just define them
-- above, and add a new `quickCheck` line below.
main :: IO ()
main = do
    quickCheck prop_testLiteralNumber
    quickCheck prop_testLiteralError
    quickCheck prop_testAddition
    quickCheck prop_testAddition2
    quickCheck prop_testBadAddition
    quickCheck prop_testBasicIdentifier
    quickCheck prop_testEqual1
    quickCheck prop_testEqual2
    quickCheck prop_testIf1
    quickCheck prop_testIf2
    quickCheck prop_testCons1
    quickCheck prop_testCons2
    quickCheck prop_testFirst1
    quickCheck prop_testFirst2
    quickCheck prop_testFirst3
    quickCheck prop_testFirst4
    quickCheck prop_testRest1
    quickCheck prop_testRest2
    quickCheck prop_testFunctionApplication
    quickCheck prop_testMultiplication
