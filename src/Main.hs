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

{- | Takes a latex expression string and parse and interpret it. The function
 - outputs the result of interpreting the program or the error encountered
 - during interpretation. -}
runProgram :: String
    -- ^ String to parse and interpret.
    -> IO ()
runProgram p = case parseString p >>= interpret of
    Left err -> print err
    Right res -> print res
