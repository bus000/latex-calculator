module ASTree where

import Data.Ratio
    ( (%)
    , Rational
    , numerator
    , denominator
    )
import Numeric.IEEE
    ( epsilon
    )
import Test.QuickCheck.Arbitrary
    ( Arbitrary
    , arbitrary
    , shrink
    )
import Test.QuickCheck
    ( sized
    , scale
    , resize
    )
import Test.QuickCheck.Gen
    ( oneof
    , frequency
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

instance Arbitrary Expr where
    arbitrary = sized expr
      where
        expr 0 = arbitrary >>= return . Literal
        expr n | n > 0 = oneof
            [ expr half >>= \a -> expr half >>= \b -> return $ Sum a b
            , expr half >>= \a -> expr half >>= \b -> return $ Product a b
            , expr half >>= \a -> expr half >>= \b -> return $ Fraction a b
            , expr half >>= \a -> expr half >>= \b -> return $ Minus a b
            , expr half >>= \a -> return $ Factorial a
            , expr half >>= \a -> expr half >>= \b -> return $ Power a b
            , arbitrary >>= \a -> return $ Literal a
            , expr half >>= \a -> expr half >>= \b -> return $ Binomial a b
            ]
          where
            half = n `div` 2

    shrink (Sum a b) = [a, b]
    shrink (Product a b) = [a, b]
    shrink (Fraction a b) = [a, b]
    shrink (Minus a b) = [a, b]
    shrink (Factorial a) = [a]
    shrink (Power a b) = [a, b]
    shrink (Binomial a b) = [a, b]

instance Eq Number where
    (Whole a) == (Whole b) = a == b
    (Real a) == (Real b) = abs (a - b) < epsilon
    (Ratio a) == (Ratio b) = a == b
    a == b = let (a', b') = toSame (a, b) in a' == b'

instance Num Number where
    (Whole a) + (Whole b) = Whole $ a + b
    (Real a) + (Real b) = Real $ a + b
    (Ratio a) + (Ratio b) = Ratio $ a + b
    a + b = let (a', b') = toSame (a, b) in a' + b'

    (Whole a) * (Whole b) = Whole $ a * b
    (Real a) * (Real b) = Real $ a * b
    (Ratio a) * (Ratio b) = Ratio $ a * b
    a * b = let (a', b') = toSame (a, b) in a' * b'

    (Whole a) - (Whole b) = Whole $ a - b
    (Real a) - (Real b) = Real $ a - b
    (Ratio a) - (Ratio b) = Ratio $ a - b
    a - b = let (a', b') = toSame (a, b) in a' - b'

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
    (Whole a) / (Whole b) = Ratio (a % b)
    (Real a) / (Real b) = Real $ a / b
    (Ratio a) / (Ratio b) = Ratio (a / b)
    a / b = let (a', b') = toSame (a, b) in a' / b'

    fromRational r = Real $ fromRational r

instance Arbitrary Number where
    arbitrary = oneof
        [ arbitrary >>= return . Whole
        , arbitrary >>= return . Real
        , arbitrary >>= return . Ratio
        ]

    shrink (Real n) = [Ratio $ realToFrac n]
    shrink (Ratio n) = [Whole $ numerator n `div` denominator n]
    shrink (Whole _) = []

instance Show Number where
    show n = myShow $ simplify n

myShow :: Number -> String
myShow (Real d) = show d
myShow (Whole n) = show n
myShow (Ratio r) = "\\frac{" ++ show n ++ "}{" ++ show d ++ "}"
  where
    n = numerator r
    d = denominator r

toSame :: (Number, Number) -> (Number, Number)
toSame (n1, n2) = case (simplify n1, simplify n2) of
    (Whole a, Whole b) -> (Whole a, Whole b)
    (Whole a, Real b) -> (Real $ fromIntegral a, Real b)
    (Whole a, Ratio b) -> (Ratio (a % 1), Ratio b)

    (Real a, Whole b) -> (Real a, Real $ fromIntegral b)
    (Real a, Real b) -> (Real a, Real b)
    (Real a, Ratio b) -> (Real a, Real $ fromRational b)

    (Ratio a, Whole b) -> (Ratio a, Ratio (b % 1))
    (Ratio a, Real b) -> (Real $ fromRational a, Real b)
    (Ratio a, Ratio b) -> (Ratio a, Ratio b)

pow :: Number -> Number -> Maybe Number
pow (Whole a) (Whole b)
    | b >= 0 = return $ Whole (a^b)
    | otherwise = (Real $ fromInteger a) `pow` (Real $ fromInteger b)
pow (Real a) (Whole b) = return $ Real $ a^^b
pow (Real a) (Real b)
    | a >= 0 = return $ Real $ a**b
    | otherwise = Nothing
pow (Ratio a) (Whole b) = return $ Ratio $ a^^b
pow (Ratio a) (Ratio b) = Real (fromRational a) `pow` Real (fromRational b)
pow a b = let (a', b') = toSame (a, b) in a' `pow` b'

fact :: Number -> Maybe Number
fact n = fact' $ simplify n
  where
    fact' (Whole n)
        | n == 0 = Just $ Whole 1
        | n >= 0 = Just $ Whole (product [1..n])
        | otherwise = Nothing
    fact' _ = Nothing

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
