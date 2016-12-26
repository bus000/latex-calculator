module Parser.Internal where

import Control.Applicative
    ( (<|>)
    )
import ASTree
    ( Number(..)
    , Expr(..)
    , LatCalError(..)
    )
import Text.ParserCombinators.ReadP
    ( ReadP
    , readP_to_S
    , optional
    , eof
    , char
    , munch
    , (<++)
    , chainl1
    , string
    , between
    , skipSpaces
    , option
    , satisfy
    )
import Data.Char
    ( isDigit
    )

{- | Parse a latex expression string to either a parseerror or a latex
 - expression. -}
parseString :: String
    -- ^ String to parse.
    -> Either LatCalError Expr
parseString str = case readP_to_S parseExpr str of
    [(program, "")] -> Right program
    _ -> Left $ ParserError "Parse error"

parseExpr :: ReadP Expr
parseExpr = do
    expr <- token parseExpr0
    eof

    return expr

parseExpr0 :: ReadP Expr
parseExpr0 = parseExpr1 `chainl1` plusminus
  where
    plusminus = plus <|> minus
    plus = infixOp "+" Sum
    minus = infixOp "-" Minus

parseExpr1 :: ReadP Expr
parseExpr1 = parseExpr2 `chainl1` mult
  where
    mult = mult1 <|> mult2 <|> mult3
    mult1 = token (optional $ char '*') >> return Product
    mult2 = infixOp "\\cdot" Product
    mult3 = infixOp "\\times" Product

parseExpr2 :: ReadP Expr
parseExpr2 = parseExpr3 `chainl1` pow
  where
    pow = infixOp "^" Power

parseExpr3 :: ReadP Expr
parseExpr3 = fact <|> parseExpr4 <|> divi <|> binom <|> expo
  where
    divi = do
        _ <- string "\\frac"
        expr1 <- between (token $ char '{') (token $ char '}') parseExpr0
        expr2 <- between (token $ char '{') (token $ char '}') parseExpr0

        return $ Fraction expr1 expr2

    fact = do
        expr <- token parseExpr4
        _ <- char '!'

        return $ Factorial expr

    binom = do
        _ <- token $ string "\\binom"
        expr1 <- between (token $ char '{') (token $ char '}') parseExpr0
        expr2 <- between (token $ char '{') (token $ char '}') parseExpr0

        return $ Binomial expr1 expr2

    expo = do
        _ <- token $ string "exp"
        expr <- between (token $ char '(') (token $ char ')') parseExpr0

        return $ Power (Literal $ Real (exp 1)) expr

parseExpr4 :: ReadP Expr
parseExpr4 = real <++ whole <++ constant <++ bracket
  where
    real = token $ do
        skipSpaces
        minus <- option ' ' (char '-')
        first <- satisfy (`elem` ['0'..'9'])
        digits1 <- munch isDigit
        _ <- char '.'
        digits2 <- munch isDigit

        let int = Whole $ read (minus:first:digits1)
            frac = Real $ read ((minus:"0.") ++ digits2)
        return $ Literal (int + frac)

    whole = do
        skipSpaces
        minus <- option ' ' (char '-')
        first <- satisfy (`elem` ['0'..'9'])
        digits <- munch isDigit

        return $ Literal (Whole $ read (minus:first:digits))

    bracket = bracket1 <|> bracket2 <|> bracket3
    bracket1 = parseBracket (char '(') (char ')')
    bracket2 = parseBracket (string "\\left(") (string "\\right)")
    bracket3 = parseBracket (char '{') (char '}')
    parseBracket s e = between (token s) (token e) parseExpr0

    constant = pi' <|> euler
    pi' = token (string "\\pi") >> return (Literal $ Real pi)
    euler = token (char 'e') >> return (Literal $ Real (exp 1))

infixOp :: String -> (a -> a -> a) -> ReadP (a -> a -> a)
infixOp x f = token (string x) >> return f

token :: ReadP a -> ReadP a
token f = skipSpaces >> f >>= \v -> skipSpaces >> return v
