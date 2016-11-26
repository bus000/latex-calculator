module ASTree where

data Expr
    = Sum Expr Expr
    | Product Expr Expr
    | Fraction Expr Expr
    | Minus Expr Expr
    | Factorial Expr
    | Literal Number
    deriving (Show, Eq)

data Number = Real Double | Whole Integer

epsilon :: Double
epsilon = 0.000001

instance Eq Number where
    (Whole i1) == (Whole i2) = i1 == i2
    (Whole i1) == (Real d1) = (Real $ fromIntegral i1) == Real d1
    (Real d1) == (Whole i1) = Real d1 == (Real $ fromIntegral i1)
    (Real d1) == (Real d2) = abs (d1 - d2) < epsilon

instance Num Number where
    (Whole i1) + (Whole i2) = Whole $ i1 + i2
    (Whole i1) + (Real d1) = Real $ fromIntegral i1 + d1
    (Real d1) + (Whole i1) = Real $ d1 + fromIntegral i1
    (Real d1) + (Real d2) = Real $ d1 + d2

    (Whole i1) * (Whole i2) = Whole $ i1 * i2
    (Whole i1) * (Real d1) = Real $ fromIntegral i1 * d1
    (Real d1) * (Whole i1) = Real $ d1 * fromIntegral i1
    (Real d1) * (Real d2) = Real $ d1 * d2

    (Whole i1) - (Whole i2) = Whole $ i1 - i2
    (Whole i1) - (Real d1) = Real $ fromIntegral i1 - d1
    (Real d1) - (Whole i1) = Real $ d1 - fromIntegral i1
    (Real d1) - (Real d2) = Real $ d1 - d2

    abs (Whole i) = Whole $ abs i
    abs (Real d) = Real $ abs d

    signum (Whole i) = Whole $ signum i
    signum (Real d) = Real $ signum d

    fromInteger = Whole

    negate (Whole i) = Whole $ negate i
    negate (Real d) = Real $ negate d

instance Fractional Number where
    (Whole i1) / (Whole i2) = (Real $ fromIntegral i1) / (Real $ fromIntegral i2)
    (Whole i1) / (Real d1) = (Real $ fromIntegral i1) / Real d1
    (Real d1) / (Whole i1) = Real d1 / (Real $ fromIntegral i1)
    (Real d1) / (Real d2) = Real $ d1 / d2

    fromRational r = Real $ fromRational r

instance Show Number where
    show (Real d) = show d
    show (Whole n) = show n
