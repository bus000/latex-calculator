module Main where

import System.Environment
    ( getArgs
    )

import Parser.Impl
    ( parseString
    )

import Interpreter.Impl
    ( interpret
    )

main :: IO ()
main = do
    args <- getArgs

    case args of
        [str] -> case parseString str of
            Right program -> print $ interpret program
            Left err -> putStrLn err
        _ -> putStrLn "Expect a single argument"
