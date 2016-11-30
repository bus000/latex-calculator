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
        [str] -> runProgram str
        _ -> getLine >>= \line -> runProgram line

runProgram :: String -> IO ()
runProgram p = case parseString p >>= interpret of
    Left err -> print err
    Right res -> print res
