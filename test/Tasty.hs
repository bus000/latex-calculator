module Main where

import qualified Interpreter.Tests as IT
import qualified Parser.Tests as PT
import Test.Tasty

allTests :: TestTree
allTests = testGroup "Tasty Tests"
    [ IT.unitTests
    , IT.qcTests
    , PT.unitTests
    ]

main :: IO ()
main = defaultMain allTests
