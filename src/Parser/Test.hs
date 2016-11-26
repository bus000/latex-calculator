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

    , testCase "fraction1" fraction1
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

fraction1 :: Assertion
fraction1 = assertBool "" $ result `elem` readP_to_S parseExpr2 program
  where
    result = (Fraction (Literal $ Whole 10) (Literal $ Whole 20), "")
    program = "\\frac{10}{20}"


main :: IO ()
main = defaultMain $ testGroup "Parser Tests" [ unitTests ]
