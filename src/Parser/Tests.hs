module Interpreter.Tests where

import ASTree
import Parser.Impl

import Test.Tasty
import Test.Tasty.HUnit
    ( testCase
    , (@=?)
    , assertBool
    , Assertion
    )
import Text.ParserCombinators.ReadP
    ( readP_to_S
    )

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "literal1" literal1
    , testCase "literal2" literal2
    , testCase "literal3" literal3
    , testCase "literal4" literal4

    , testCase "product1" product1
    , testCase "product2" product2
    , testCase "product3" product3
    , testCase "product4" product4

    , testCase "fraction1" fraction1
    , testCase "fraction2" fraction2

    , testCase "factorial1" factorial1
    , testCase "factorial2" factorial2

    , testCase "expr1" expr1
    , testCase "expr2" expr2
    , testCase "expr3" expr3
    , testCase "expr4" expr4
    , testCase "expr5" expr5
    ]

literal1 :: Assertion
literal1 = [(Literal $ Whole 10, "")] @=? readP_to_S parseExpr2 "10"

literal2 :: Assertion
literal2 = [(Literal $ Real 10, "")] @=? readP_to_S parseExpr2 "10.0"

literal3 :: Assertion
literal3 = [(Literal $ Whole 10, "  ")] @=? readP_to_S parseExpr2 "  10  "

literal4 :: Assertion
literal4 = [(Literal $ Real 124.001, "")] @=? readP_to_S parseExpr2 "  124.001"

product1 :: Assertion
product1 = assertBool "" $ result `elem` readP_to_S parseExpr1 program
  where
    result = (Product (Literal $ Whole 10) (Literal $ Whole 20), "")
    program = "10 * 20"

product2 :: Assertion
product2 = assertBool "" $ result `elem` readP_to_S parseExpr1 program
  where
    result = (Product (Product (Product (Product
        (Literal $ Whole 1) (Literal $ Whole 2)) (Literal $ Whole 3))
        (Literal $ Whole 4)) (Literal $ Whole 5), " ")
    program = " 1 * 2 * 3 * 4 * 5 "

product3 :: Assertion
product3 = assertBool "" $ result `elem` readP_to_S parseExpr1 program
  where
    result = (Product (Literal $ Whole 10) (Literal $ Whole 20), "")
    program = "10 \\cdot 20"

product4 :: Assertion
product4 = assertBool "" $ result `elem` readP_to_S parseExpr1 program
  where
    result = (Product (Literal $ Whole 10) (Literal $ Whole 20), "")
    program = "10 \\times 20"

fraction1 :: Assertion
fraction1 = assertBool "" $ result `elem` readP_to_S parseExpr2 program
  where
    result = (Fraction (Literal $ Whole 10) (Literal $ Whole 20), "")
    program = "\\frac{10}{20}"

fraction2 :: Assertion
fraction2 = assertBool "" $ result `elem` readP_to_S parseExpr2 program
  where
    result = (Fraction (Literal $ Whole 10) (Literal $ Whole 20), "")
    program = "\\frac  { 10\t}   {  20 }  "

factorial1 :: Assertion
factorial1 = result @=? readP_to_S parseExpr2 program
  where
    result = [(Factorial (Literal $ Whole 5), "")]
    program = "5!"

factorial2 :: Assertion
factorial2 = result @=? readP_to_S parseExpr program
  where
    result = [(Sum (Literal $ Whole 5) (Factorial (Literal $ Whole 6)), "")]
    program = "5 + 6!"

expr1 :: Assertion
expr1 = Right result @=? parseString program
  where
    result = (Sum (Sum (Literal $ Whole 2) (Product (Literal $ Whole 3)
        (Literal $ Whole 4))) (Literal $ Whole 5))
    program = " 2 + 3   * 4  +  5   "

expr2 :: Assertion
expr2 = Right result @=? parseString program
  where
    result = Fraction over under
    over = fact10
    under = Product fact10
        (Factorial (Minus (Literal $ Whole 10) (Literal $ Whole 5)))
    fact10 = Factorial (Literal $ Whole 10)
    program = "\\frac{10!}{10! \\cdot (10 - 5)!}"

expr3 :: Assertion
expr3 = Right result @=? parseString program
  where
    result = Product (Literal $ Whole 2)
        (Sum (Literal $ Whole 5) (Literal $ Whole 4))
    program = "2 * (5 + 4)"

expr4 :: Assertion
expr4 = Right result @=? parseString program
  where
    result = Sum (Literal $ Whole 5) (Literal $ Whole 10)
    program = "(5 + 10)"

expr5 :: Assertion
expr5 = Right result @=? parseString program
  where
    result = Literal $ Whole 5
    program = "(5)"

main :: IO ()
main = defaultMain $ testGroup "Parser Tests" [ unitTests ]
