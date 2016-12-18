module Interpreter.Impl where

import ASTree
import Math.Combinatorics.Exact.Binomial
    ( choose
    )

interpret :: Expr -> Either LatCalError Number
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
    case simplify a of
        Whole n -> return $ Whole (fact n)
        _ -> Left $ TypeError "Factorial of non whole number"

interpret (Power e1 e2) = do
    a <- interpret e1
    b <- interpret e2

    case a `pow` b of
        Just n -> return n
        Nothing -> Left $ TypeError "" -- TODO: Message

interpret (Binomial e1 e2) = do
    a <- interpret e1
    b <- interpret e2

    case (simplify a, simplify b) of
        (Whole n, Whole k) -> return $ Whole (n `choose` k)
        _ -> Left $ TypeError "Binomial of non whole number"

interpret (Literal num) = return num

fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact (n - 1)
