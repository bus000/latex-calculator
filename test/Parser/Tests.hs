module Parser.Tests
    ( tests
    ) where

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

tests :: TestTree
tests = testGroup "Parser Tests"
    [ unitTests
    ]

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
    , testCase "expr6" expr6
    , testCase "expr7" expr7
    , testCase "expr8" expr8
    , testCase "expr9" expr9
    , testCase "expr10" expr10
    , testCase "expr11" expr11
    , testCase "expr12" expr12
    , testCase "expr13" expr13
    , testCase "expr14" expr14
    , testCase "expr15" expr15
    , testCase "expr16" expr16
    , testCase "expr17" expr17
    , testCase "expr18" expr18
    , testCase "expr19" expr19
    ]

literal1 :: Assertion
literal1 = [(ten, "")] @=? readP_to_S parseExpr2 "10"

literal2 :: Assertion
literal2 = [(ten, "")] @=? readP_to_S parseExpr2 "10.0"

literal3 :: Assertion
literal3 = [(ten, "  ")] @=? readP_to_S parseExpr2 "  10  "

literal4 :: Assertion
literal4 = [(Literal $ Real 124.001, "")] @=? readP_to_S parseExpr2 "  124.001"

product1 :: Assertion
product1 = assertBool "" $ result `elem` readP_to_S parseExpr1 program
  where
    result = (Product ten (Literal $ Whole 20), "")
    program = "10 * 20"

product2 :: Assertion
product2 = assertBool "" $ result `elem` readP_to_S parseExpr1 program
  where
    result = (Product (Product (Product (Product one two) three) four) five, "")
    program = " 1 * 2 * 3 * 4 * 5"

product3 :: Assertion
product3 = assertBool "" $ result `elem` readP_to_S parseExpr1 program
  where
    result = (Product ten (Literal $ Whole 20), "")
    program = "10 \\cdot 20"

product4 :: Assertion
product4 = assertBool "" $ result `elem` readP_to_S parseExpr1 program
  where
    result = (Product ten (Literal $ Whole 20), "")
    program = "10 \\times 20"

fraction1 :: Assertion
fraction1 = assertBool "" $ result `elem` readP_to_S parseExpr2 program
  where
    result = (Fraction ten (Literal $ Whole 20), "")
    program = "\\frac{10}{20}"

fraction2 :: Assertion
fraction2 = assertBool "" $ result `elem` readP_to_S parseExpr2 program
  where
    result = (Fraction ten (Literal $ Whole 20), "")
    program = "\\frac  { 10\t}   {  20 }  "

factorial1 :: Assertion
factorial1 = assertBool "" $ result `elem` readP_to_S parseExpr2 program
  where
    result = (Factorial (Literal $ Whole 5), "")
    program = "5!"

factorial2 :: Assertion
factorial2 = result @=? readP_to_S parseExpr program
  where
    result = [(Sum five (Factorial six), "")]
    program = "5 + 6!"

expr1 :: Assertion
expr1 = Right result @=? parseString program
  where
    result = Sum (Sum two (Product three four)) five
    program = " 2 + 3   * 4  +  5   "

expr2 :: Assertion
expr2 = Right result @=? parseString program
  where
    result = Fraction over under
    over = fact10
    under = Product fact10
        (Factorial (Minus (Literal $ Whole 10) (Literal $ Whole 5)))
    fact10 = Factorial ten
    program = "\\frac{10!}{10! \\cdot (10 - 5)!}"

expr3 :: Assertion
expr3 = Right result @=? parseString program
  where
    result = Product two (Sum five four)
    program = "2 * (5 + 4)"

expr4 :: Assertion
expr4 = Right result @=? parseString program
  where
    result = Sum five ten
    program = "(5 + 10)"

expr5 :: Assertion
expr5 = Right result @=? parseString program
  where
    result = five
    program = "(5)"

expr6 :: Assertion
expr6 = Right result @=? parseString program
  where
    result = Factorial (Product two (Sum five four))
    program = "(2 * \\left(5 + 4\\right))!"

expr7 :: Assertion
expr7 = Right result @=? parseString program
  where
    result = Fraction over under
    over = Minus one (Fraction one two)
    under = Literal $ Whole 0
    program = "\\frac{1 - \\left( \\frac{1}{2} \\right)}{0}"

expr8 :: Assertion
expr8 = Right result @=? parseString program
  where
    result = Fraction one two
    program = "\\left( \\frac{1}{2} \\right)"

expr9 :: Assertion
expr9 = Right result @=? parseString program
  where
    result = Minus one (Fraction one two)
    program = "1 - \\left( \\frac{1}{2} \\right)\n"

expr10 :: Assertion
expr10 = Right result @=? parseString program
  where
    result = Power two (Power two two)
    program = "2\t^(2 ^   \n 2)\n"

expr11 :: Assertion
expr11 = Right result @=? parseString program
  where
    result = Power base expo
    program = "\\left( \\frac{1}{2} \\right)^{10}"
    base = Fraction one two
    expo = ten

expr12 :: Assertion
expr12 = Right result @=? parseString program
  where
    result = Binomial ten five
    program = "\\binom{10}{5}"

expr13 :: Assertion
expr13 = Right result @=? parseString program
  where
    result = Product (Binomial ten five) (Power (Fraction one two) five)
    program = "\\binom{10}{5} \\left( \\frac{1}{2} \\right)^5"

expr14 :: Assertion
expr14 = Right result @=? parseString program
  where
    result = Power base expo
    base = Literal $ Real (exp 1)
    expo = Product (Product (Literal $ Whole (-10)) ten)
        (Power (Fraction one two) two)
    program = "exp(-10 \\cdot 10 \\frac{1}{2}^2)"

expr15 :: Assertion
expr15 = Right result @=? parseString program
  where
    result = Product (Literal $ Real 2.718281828459045) (Literal $ Real pi)
    program = " e * \\pi "

expr16 :: Assertion
expr16 = Right result @=? parseString program
  where
    result = Power (Literal $ Real (exp 1)) (Product two four)
    program = " exp  ( \n \t 2 \\times 4 \n )\n  "

expr17 :: Assertion
expr17 = Right result @=? parseString program
  where
    result = Literal $ Whole (-10)
    program = " -10"

expr18 :: Assertion
expr18 = Right result @=? parseString program
  where
    result = Product (Power five ten) six
    program = " 5 ^ 10 6"

expr19 :: Assertion
expr19 = Right result @=? parseString program
  where
    result = Power five (Product ten six)
    program = " \t 5 ^ (10 * 6)  "

zero, one, two, three, four, five, six, seven, eight, nine, ten :: Expr
zero = Literal $ Whole 0
one = Literal $ Whole 1
two = Literal $ Whole 2
three = Literal $ Whole 3
four = Literal $ Whole 4
five = Literal $ Whole 5
six = Literal $ Whole 6
seven = Literal $ Whole 7
eight = Literal $ Whole 8
nine = Literal $ Whole 9
ten = Literal $ Whole 10
