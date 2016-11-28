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
import ASTree
    ( Number
    )

main :: IO ()
main = do
    args <- getArgs

    case args of
        [str] -> runProgram str
        _ -> getLine >>= \line -> runProgram line

runProgram :: String -> IO ()
runProgram p = case parseInterpret p of
    Left err -> putStrLn err
    Right res -> print res

parseInterpret :: String -> Either String Number
parseInterpret p = do
    program <- parseString p
    interpret program
