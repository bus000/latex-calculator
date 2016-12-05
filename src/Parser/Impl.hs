module Parser.Impl where

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

parseString :: String -> Either LatCalError Expr
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
parseExpr2 = fact <|> parseExpr3 <|> divi <|> pow <|> binom
  where
    divi = do
        _ <- string "\\frac"
        expr1 <- between (token $ char '{') (token $ char '}') parseExpr0
        expr2 <- between (token $ char '{') (token $ char '}') parseExpr0

        return $ Fraction expr1 expr2

    fact = do
        expr <- token parseExpr3
        _ <- char '!'

        return $ Factorial expr

    pow = do
        expr1 <- token parseExpr3
        _ <- char '^'
        expr2 <- parseExpr3

        return $ Power expr1 expr2

    binom = do
        _ <- token $ string "\\binom"
        expr1 <- between (token $ char '{') (token $ char '}') parseExpr0
        expr2 <- between (token $ char '{') (token $ char '}') parseExpr0

        return $ Binomial expr1 expr2


parseExpr3 :: ReadP Expr
parseExpr3 = real <++ whole <++ constant <++ bracket
  where
    real = token $ do
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
