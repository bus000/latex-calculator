module ASTree where

import Data.Ratio
    ( (%)
    , Rational
    , numerator
    , denominator
    )

data Expr
    = Sum Expr Expr
    | Product Expr Expr
    | Fraction Expr Expr
    | Minus Expr Expr
    | Factorial Expr
    | Power Expr Expr
    | Literal Number
    | Binomial Expr Expr
    deriving (Show, Eq)

data Number = Real Double | Whole Integer | Ratio Rational

epsilon :: Double
epsilon = 0.000001

instance Eq Number where
    (Whole i1) == (Whole i2) = i1 == i2
    (Whole i1) == (Real d1) = toReal (Whole i1) == Real d1
    (Whole i1) == (Ratio r1) = Ratio (i1 % 1) == Ratio r1

    (Real d1) == (Whole i1) = Real d1 == toReal (Whole i1)
    (Real d1) == (Real d2) = abs (d1 - d2) < epsilon
    (Real d1) == (Ratio r1) = Real d1 == toReal (Ratio r1)

    (Ratio r) == (Whole i) = Ratio r == Ratio (i % 1)
    (Ratio r) == (Real d) = toReal (Ratio r) == Real d
    (Ratio r1) == (Ratio r2) = r1 == r2

instance Num Number where
    (Whole i1) + (Whole i2) = Whole $ i1 + i2
    (Whole i1) + (Real d1) = Real $ fromIntegral i1 + d1
    (Whole i1) + (Ratio r1) = Ratio (i1 % 1) + Ratio r1
    (Real d1) + (Whole i1) = Real $ d1 + fromIntegral i1
    (Real d1) + (Real d2) = Real $ d1 + d2
    (Real d1) + (Ratio r1) = Real d1 + toReal (Ratio r1)
    (Ratio r1) + (Whole i2) = Ratio r1 + Ratio (i2 % 1)
    (Ratio r1) + (Real d2) = toReal (Ratio r1) + Real d2
    (Ratio r1) + (Ratio r2) = Ratio $ r1 + r2

    (Whole i1) * (Whole i2) = Whole $ i1 * i2
    (Whole i1) * (Real d1) = Real $ fromIntegral i1 * d1
    (Whole i1) * (Ratio r1) = Ratio (i1 % 1) * Ratio r1
    (Real d1) * (Whole i1) = Real $ d1 * fromIntegral i1
    (Real d1) * (Real d2) = Real $ d1 * d2
    (Real d1) * (Ratio r1) = Real d1 * toReal (Ratio r1)
    (Ratio r1) * (Whole i2) = Ratio r1 * Ratio (i2 % 1)
    (Ratio r1) * (Real d2) = toReal (Ratio r1) * Real d2
    (Ratio r1) * (Ratio r2) = Ratio $ r1 * r2

    (Whole i1) - (Whole i2) = Whole $ i1 - i2
    (Whole i1) - (Real d1) = Real $ fromIntegral i1 - d1
    (Whole i1) - (Ratio r1) = Ratio (i1 % 1) - Ratio r1
    (Real d1) - (Whole i1) = Real $ d1 - fromIntegral i1
    (Real d1) - (Real d2) = Real $ d1 - d2
    (Real d1) - (Ratio r1) = Real d1 - toReal (Ratio r1)
    (Ratio r1) - (Whole i2) = Ratio r1 - Ratio (i2 % 1)
    (Ratio r1) - (Real d2) = toReal (Ratio r1) - Real d2
    (Ratio r1) - (Ratio r2) = Ratio $ r1 - r2

    abs (Whole i) = Whole $ abs i
    abs (Real d) = Real $ abs d
    abs (Ratio r) = Ratio $ abs r

    signum (Whole i) = Whole $ signum i
    signum (Real d) = Real $ signum d
    signum (Ratio r) = Ratio $ signum r

    fromInteger = Whole

    negate (Whole i) = Whole $ negate i
    negate (Real d) = Real $ negate d
    negate (Ratio r) = Ratio $ negate r

instance Fractional Number where
    (Whole i1) / (Whole i2) = Ratio (i1 % i2)
    (Whole i1) / (Real d1) = (Real $ fromIntegral i1) / Real d1
    (Whole i1) / (Ratio r1) = Ratio (i1 % 1) / Ratio r1
    (Real d1) / (Whole i1) = Real d1 / (Real $ fromIntegral i1)
    (Real d1) / (Real d2) = Real $ d1 / d2
    (Real d1) / (Ratio r1) = Real d1 / toReal (Ratio r1)
    (Ratio r1) / (Whole i2) = Ratio r1 / Ratio (i2 % 1)
    (Ratio r1) / (Real d2) = toReal (Ratio r1) / Real d2
    (Ratio r1) / (Ratio r2) = Ratio (r1 / r2)

    fromRational r = Real $ fromRational r

instance Show Number where
    show n = myShow $ simplify n

myShow :: Number -> String
myShow (Real d) = show d
myShow (Whole n) = show n
myShow (Ratio r) = "\\frac{" ++ show n ++ "}{" ++ show d ++ "}"
  where
    n = numerator r
    d = denominator r

toReal :: Number -> Number
toReal (Whole n) = Real $ fromIntegral n
toReal (Real d) = Real d
toReal (Ratio r) =
    Real $ fromIntegral (numerator r) / fromIntegral (denominator r)

pow :: Number -> Number -> Number
pow (Whole n1) (Whole n2)
    | n2 >= 0 = Whole (n1^n2)
    | otherwise = (Real $ fromInteger n1) `pow` (Real $ fromInteger n2)
pow (Whole n1) (Real n2) = Real (fromIntegral n1**n2)
pow (Whole n1) (Ratio n2) = toReal (Whole n1) `pow` toReal (Ratio n2)
pow (Real n1) (Whole n2) = Real $ n1^^n2
pow (Real n1) (Real n2) = Real $ n1**n2
pow (Real n1) (Ratio n2) = Real n1 `pow` toReal (Ratio n2)
pow (Ratio n1) (Whole n2) = Ratio $ n1^^n2
pow (Ratio n1) (Real n2) = toReal (Ratio n1) `pow` Real n2
pow (Ratio n1) (Ratio n2) = toReal (Ratio n1) `pow` toReal (Ratio n2)

simplify :: Number -> Number
simplify (Ratio r) = if n `mod` d == 0 then Whole (n `div` d) else Ratio r
  where
    n = numerator r
    d = denominator r
simplify n = n

data LatCalError
    = TypeError String
    | DivideByZero
    | ParserError String
    deriving (Show)

instance Eq LatCalError where
    TypeError _ == TypeError _ = True
    DivideByZero == DivideByZero = True
    ParserError _ == ParserError _ = True
    _ == _ = False
