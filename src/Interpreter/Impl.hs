module Interpreter.Impl where

import ASTree
import Math.Combinatorics.Exact.Binomial
    ( choose
    )
import Data.List
    ( foldl'
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

pow :: Number -> Number -> Either LatCalError Number
pow (Whole a) (Whole b)
    | b >= 0 = return $ Whole (a^b)
    | otherwise = (Real $ fromInteger a) `pow` (Real $ fromInteger b)
pow (Real a) (Whole b) = return $ Real $ a^^b
pow (Real a) (Real b)
    | a > 0 = return $ Real $ a**b
    | otherwise = Left $ TypeError "" -- TODO: Message
pow (Ratio a) (Whole b) = return $ Ratio $ a^^b
pow (Ratio a) (Ratio b) = Real (fromRational a) `pow` Real (fromRational b)
pow a b = let (a', b') = toSame (a, b) in a' `pow` b'

fact :: Number -> Either LatCalError Number
fact n = fact' $ simplify n
  where
    fact' (Whole n)
        | n == 0 = return $ Whole 1
        | n >= 0 = return $ Whole (foldl' (*) 1 [1..n])
        | otherwise = Left $ TypeError "Factorial of negative number"
    fact' _ = Left $ TypeError "Factorial of non whole number"
