module Parser.Impl where

import Control.Applicative
    ( (<|>)
    )
import ASTree
import Text.ParserCombinators.ReadP
    ( ReadP
    , readP_to_S
    , many
    , eof
    , char
    , munch
    , munch1
    , many1
    , (<++)
    , pfail
    , look
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

type ParseError = String

parseString :: String -> Expr
parseString = undefined

parseExpr :: ReadP Expr
parseExpr = do
    expr <- parseExpr0
    eof

    return expr

parseExpr0 :: ReadP Expr
parseExpr0 = parseExpr1 `chainl1` plusminus
  where
    plusminus = plus <|> minus
    plus = infixOp "+" Sum
    minus = infixOp "-" Minus

parseExpr1 :: ReadP Expr
parseExpr1 = parseExpr2 `chainl1` muldiv
  where
    muldiv = mult <|> divi
    mult = infixOp "*" Product
    divi = do
        string "\\frac"
        expr1 <- between (token $ char '{') (token $ char '}') parseExpr0
        expr2 <- between (token $ char '{') (token $ char '}') parseExpr0

        return $ Fraction expr1 expr2

parseExpr3 :: ReadP Expr
parseExpr3 = real <++ whole
  where
    real = do
        skipSpaces
        minus <- option ' ' (char '-')
        first <- satisfy (`elem` ['1'..'9'])
        digits1 <- munch isDigit
        char '.'
        digits2 <- munch isDigit

        let int = Whole $ read (minus:first:digits1)
            frac = Real $ read ((minus:"0.") ++ digits2)
        return $ Literal (int + frac)

    whole = do
        skipSpaces
        minus <- option ' ' (char '-')
        digits <- munch isDigit

        return $ Literal (Whole $ read (minus:digits))

infixOp :: String -> (a -> a -> a) -> ReadP (a -> a -> a)
infixOp x f = token (string x) >> return f

token :: ReadP a -> ReadP a
token f = skipSpaces >> f >>= \v -> skipSpaces >> return v
