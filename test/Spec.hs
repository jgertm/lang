module Main where

import           Test.Tasty

import qualified Test.Interpreter as Interpreter
import qualified Test.Parser      as Parser
import qualified Test.Renaming    as Renaming
import qualified Test.Types       as Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Unit tests"
    [Renaming.tree, Interpreter.tree, Parser.tree, Types.tree]
