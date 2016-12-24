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
    )
import Test.QuickCheck.Gen
    ( oneof
    )

{- | Abstract syntax tree of latex expressions. -}
data Expr
    = Sum !Expr !Expr -- ^ Expression representing a sum.
    | Product !Expr !Expr -- ^ Expression representing a product.
    | Fraction !Expr !Expr -- ^ Expression representing a fraction.
    | Minus !Expr !Expr -- ^ Expression representing a subtraction.
    | Factorial !Expr -- ^ Expression representing a factorial.
    | Power !Expr !Expr -- ^ Expression representing a power.
    | Literal !Number -- ^ Expression representing a literal number.
    | Binomial !Expr !Expr -- ^ Expression representing binomials.
    deriving (Show, Eq)

{- | The number type for the interpreter. Conversions between the number types
 - are performed automatically when arithmetic is done with them. For example a
 - whole number divided by a whole number will result in a Ratio. The numbers
 - try to keep precision whenever it can and will only go to a Real number when
 - it is forced to do so. When an expression has resulted in a Real number it
 - can never become Whole or Ratio again. -}
data Number
    = Real Double -- ^ Represents a real number by using Doubles.
    | Whole Integer -- ^ Represent a whole number by an infinite integer.
    | Ratio Rational -- ^ Represent a ratio by using Rational's.

{- | Arbitrary expressions to use with QuickCheck. -}
instance Arbitrary Expr where
    arbitrary = sized expr
      where
        expr 0 = fmap Literal arbitrary
        expr n = oneof
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

    shrink (Literal _) = []
    shrink (Sum a b) = [a, b]
    shrink (Product a b) = [a, b]
    shrink (Fraction a b) = [a, b]
    shrink (Minus a b) = [a, b]
    shrink (Factorial a) = [a]
    shrink (Power a b) = [a, b]
    shrink (Binomial a b) = [a, b]

{- | Whole and Ratio is compared with normal equality while Real numbers are
 - compared using the machine-epsilon. -}
instance Eq Number where
    (Whole a) == (Whole b) = a == b
    (Real a) == (Real b) = abs (a - b) < epsilon
    (Ratio a) == (Ratio b) = a == b
    a == b = let (a', b') = toSame (a, b) in a' == b'

{- | Makes number an instance of Num to be able to do arithmetic on numbers. As
 - explained earlier numbers will be converted to the same type of number as it
 - is needed. -}
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

{- | Makes Number an instance of Fractional to allow for numbers to be divided
 - by each other. -}
instance Fractional Number where
    (Whole a) / (Whole b) = Ratio (a % b)
    (Real a) / (Real b) = Real $ a / b
    (Ratio a) / (Ratio b) = Ratio (a / b)
    a / b = let (a', b') = toSame (a, b) in a' / b'

    fromRational r = Real $ fromRational r

{- | To be used with QuickCheck to generate random test cases. -}
instance Arbitrary Number where
    arbitrary = oneof
        [ fmap Whole arbitrary
        , fmap Real arbitrary
        , fmap Ratio arbitrary
        ]

    shrink (Real n) = [Ratio $ realToFrac n]
    shrink (Ratio n) = [Whole $ numerator n `div` denominator n]
    shrink (Whole _) = []

{- | Allows one to print Number's in latex syntax. -}
instance Show Number where
    show n = myShow $ simplify n

{- | Show a Number as latex writes them. -}
myShow :: Number
    -- ^ Number to show.
    -> String
myShow (Real d) = show d
myShow (Whole n) = show n
myShow (Ratio r) = "\\frac{" ++ show n ++ "}{" ++ show d ++ "}"
  where
    n = numerator r
    d = denominator r

{- | Convert to numbers to the same type of number. To perform most arithmetic
 - operations we need to have either two Whole, two Real or two Ratio's. The
 - function will first try if it can return two whole numbers if that can't be
 - done, it will try to return two Ratio's and finally it will default to
 - returning two Real numbers. -}
toSame :: (Number, Number)
    -- ^ The two Number's to take to the same type.
    -> (Number, Number)
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

{- | Simplify a Number by trying to get it to be a Whole number. -}
simplify :: Number
    -- ^ The number to simplify.
    -> Number
simplify (Ratio r) = if n `mod` d == 0 then Whole (n `div` d) else Ratio r
  where
    n = numerator r
    d = denominator r
simplify n = n

{- | Represents an error in the interpretation of an abstract syntax tree and
 - the parsing of an input string. -}
data LatCalError
    = TypeError String -- ^ Mathematical operator applied to wrong argument.
    | DivideByZero -- ^ Division by zero error.
    | ParserError String -- ^ Any error in the parser is represented here.
    deriving (Show)

{- | Compare errors. -}
instance Eq LatCalError where
    TypeError _ == TypeError _ = True
    DivideByZero == DivideByZero = True
    ParserError _ == ParserError _ = True
    _ == _ = False
