module Interpreter.Internal where

import ASTree
import Math.Combinatorics.Exact.Binomial
    ( choose
    )
import Data.List
    ( foldl'
    )

{- | Interpret a latex expression in the Either monad. The interpretation will
 - result in either a Number or a LatCalError. -}
interpret :: Expr
    -- ^ Expression to interpret.
    -> Either LatCalError Number
interpret (Sum e1 e2) = do
    a <- interpret e1
    b <- interpret e2
    return $ a + b

interpret (Product e1 e2) = do
    a <- interpret e1
    b <- interpret e2
    return $ a * b

interpret (Fraction e1 e2) = do
    a <- interpret e1
    b <- interpret e2

    if b /= Whole 0
    then return $ a / b
    else Left DivideByZero

interpret (Minus e1 e2) = do
    a <- interpret e1
    b <- interpret e2
    return $ a - b

interpret (Factorial e) = do
    a <- interpret e
    fact a

interpret (Power e1 e2) = do
    a <- interpret e1
    b <- interpret e2

    a `pow` b

interpret (Binomial e1 e2) = do
    a <- interpret e1
    b <- interpret e2

    case (simplify a, simplify b) of
        (Whole n, Whole k) -> return $ Whole (n `choose` k)
        _ -> Left $ TypeError "Binomial of non whole number"

interpret (Literal num) = return num

{- | Computes the power of two Number's, pow b n returns b^n or an error. -}
pow :: Number
    -- ^ Base of the exponentiation.
    -> Number
    -- ^ Exponent of the exponentiation.
    -> Either LatCalError Number
pow (Whole b) (Whole n)
    | n >= 0 = return $ Whole (b^n)
    | otherwise = (Real $ fromInteger b) `pow` (Real $ fromInteger n)
pow (Real b) (Whole n) = return $ Real $ b^^n
pow (Real b) (Real n)
    | b > 0 = return $ Real $ b**n
    | otherwise = Left $ TypeError "" -- TODO: Message
pow (Ratio b) (Whole n) = return $ Ratio $ b^^n
pow (Ratio b) (Ratio n) = Real (fromRational b) `pow` Real (fromRational n)
pow b n = let (b', n') = toSame (b, n) in b' `pow` n'

{- | Computes the factorial of Number's. Factorials of negative or non whole
 - numbers results in a TypeError. -}
fact :: Number
    -- ^ Number to compute factorial of.
    -> Either LatCalError Number
fact n = fact' $ simplify n
  where
    fact' (Whole n')
        | n' == 0 = return $ Whole 1
        | n' >= 0 = return $ Whole (foldl' (*) 1 [1..n'])
        | otherwise = Left $ TypeError "Factorial of negative number"
    fact' _ = Left $ TypeError "Factorial of non whole number"
