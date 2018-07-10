{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Text        (Text)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec

import           AST
import           Parser

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parser]

parser :: TestTree
parser =
  let tester p name inp out = testCase name $ parse p "" inp @?= Right out
   in testGroup
        "Parser"
        [ testGroup "Identifier" $
          let test = tester identifier
           in [ test "unqualified identifier" "foobar" $
                Identifier Nothing (Name "foobar")
              , test "singly qualified identifier" "foo/bar" $
                Identifier (Just [Name "foo"]) (Name "bar")
              , test "multiply qualified identifier" "foo.baz/bar" $
                Identifier (Just [Name "foo", Name "baz"]) (Name "bar")
              ]
        , testGroup "Name" $
          let test = tester name
           in [ test "simple name" "foobar" $ Name "foobar"
              , test "name with earmuffs" "*foo*" $ Name "*foo*"
              , tester
                  dottedNames
                  "dotted names"
                  "foo.bar"
                  [Name "foo", Name "bar"]
              ]
        , testGroup "Application" $
          let test = tester parseApplication
           in [ test "nullary function" "(foo)" $
                Application (Identifier Nothing $ Name "foo") []
              , test "binary function" "(foo bar baz)" $
                Application
                  (Identifier Nothing $ Name "foo")
                  [ Symbol (Identifier Nothing $ Name "bar")
                  , Symbol (Identifier Nothing $ Name "baz")
                  ]
              , test "nested functions" "(foo bar (baz quuz))" $
                Application
                  (Identifier Nothing $ Name "foo")
                  [ Symbol (Identifier Nothing $ Name "bar")
                  , Application
                      (Identifier Nothing $ Name "baz")
                      [Symbol (Identifier Nothing $ Name "quuz")]
                  ]
              ]
        ]
