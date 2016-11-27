module Interpreter.Tests where

import ASTree
import Interpreter.Impl

import Test.Tasty
import Test.Tasty.HUnit
    ( testCase
    , (@=?)
    , assertBool
    , Assertion
    )

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "sum1" sum1
    , testCase "sum2" sum2

    , testCase "product1" product1
    , testCase "product2" product2

    , testCase "fractional1" fractional1

    , testCase "minus1" minus1
    , testCase "minus2" minus2

    , testCase "factorial1" factorial1
    , testCase "factorial2" factorial2
    ]

sum1 :: Assertion
sum1 = Right (Whole 10) @=? interpret tree
  where
    tree = Sum (Literal $ Whole 5)
        (Sum (Literal $ Whole 3) (Literal $ Whole 2))

sum2 :: Assertion
sum2 = Right (Real 10) @=? interpret tree
  where
    tree = Sum (Literal $ Whole 5)
        (Sum (Literal $ Real 3) (Literal $ Whole 2))

product1 :: Assertion
product1 = Right (Whole 50) @=? interpret tree
  where
    tree = Product (Literal $ Whole 2)
        (Product (Literal $ Whole 5) (Literal $ Whole 5))

product2 :: Assertion
product2 = Right (Real 50) @=? interpret tree
  where
    tree = Product (Literal $ Real 2)
        (Product (Literal $ Whole 5) (Literal $ Whole 5))

fractional1 :: Assertion
fractional1 = Right (Real 2.5) @=? interpret tree
  where
    tree = Fraction (Literal $ Whole 10)
        (Fraction (Literal $ Whole 2) (Literal $ Real 0.5))

minus1 :: Assertion
minus1 = Right (Whole 10) @=? interpret tree
  where
    tree = Minus (Literal $ Whole 50)
        (Minus (Literal $ Whole 5) (Literal $ Whole (-35)))

minus2 :: Assertion
minus2 = Right (Whole 10) @=? interpret tree
  where
    tree = Minus (Literal $ Real 50)
        (Minus (Literal $ Whole 5) (Literal $ Whole (-35)))

factorial1 :: Assertion
factorial1 = Right (Whole 120) @=? interpret tree
  where
    tree = Factorial (Product (Literal $ Whole 5) (Literal $ Whole 1))

factorial2 :: Assertion
factorial2 = Left "Factorial of real 5.0" @=? interpret tree
  where
    tree = Factorial (Product (Literal $ Whole 5) (Literal $ Real 1))

main :: IO ()
main = defaultMain $ testGroup "Interpreter Tests" [ unitTests ]
