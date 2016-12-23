module Main where

import qualified Interpreter.Tests as IT
import qualified Parser.Tests as PT
import Test.Tasty
    ( TestTree
    , testGroup
    , defaultMain
    )

allTests :: TestTree
allTests = testGroup "Tasty Tests"
    [ IT.tests
    , PT.tests
    ]

main :: IO ()
main = defaultMain allTests
