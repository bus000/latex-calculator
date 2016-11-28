module Interpreter.Impl where

import ASTree
import Math.Combinatorics.Exact.Binomial
    ( choose
    )

interpret :: Expr -> Either String Number
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
    else Left "Divide by zero"

interpret (Minus e1 e2) = do
    a <- interpret e1
    b <- interpret e2
    return $ a - b

interpret (Factorial e) = do
    a <- interpret e
    case a of
        Whole n -> return $ Whole (fact n)
        Real r -> Left $ "Factorial of real " ++ show r

interpret (Power e1 e2) = do
    a <- interpret e1
    b <- interpret e2

    return $ a `pow` b

interpret (Binomial e1 e2) = do
    a <- interpret e1
    b <- interpret e2

    case (a, b) of
        (Whole n, Whole k) -> return $ Whole (n `choose` k)
        _ -> Left "Can only take binomials of integers"

interpret (Literal num) = return num

fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact (n - 1)
