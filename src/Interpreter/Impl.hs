module Interpreter.Impl where

import ASTree

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

    if b /= (Whole 0)
    then return $ a / b
    else Left $ "Divide by zero"
interpret (Minus e1 e2) = do
    a <- interpret e1
    b <- interpret e2
    return $ a - b
interpret (Factorial e) = do
    a <- interpret e
    case a of
        Whole n -> return $ Whole (fact n)
        Real r -> Left $ "Factorial of real " ++ show r
interpret (Literal num) = return num

-- TODO: implement support for \binom as haskell has a choose function

fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact (n - 1)
