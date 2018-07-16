{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Text        (Text)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec

import           Parser.Abstract  hiding (parser)
import           Syntax.Abstract

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parser]

parser :: TestTree
parser =
  let tester p name inp out = testCase name $ parse p "" inp @?= Right out
   in testGroup
        "Parser"
        [ testGroup
            "Primitives"
            [ let test = tester name
               in testGroup
                    "Lexeme"
                    [ test "compact name" "foobar" $ VSymbol "foobar"
                    , test "qualified name" "foo.bar" $ VSymbol "foo.bar"
                    ]
            ]
        , testGroup
            "Values"
            [ let test = tester vectorValue
               in testGroup
                    "Vectors"
                    [ test "empty" "[]" (VVector mempty)
                    , test "empty w/ space" "[ ]" (VVector mempty)
                    , test "unit element" "[()]" (VVector [VAtom AUnit])
                    , test
                        "numbers"
                        "[1 2 3]"
                        (VVector $ fmap (VAtom . AInteger) [1, 2, 3])
                    ]
            , testGroup
                "Pattern matches"
                [ testGroup
                    "pattern clauses"
                    [ let test = tester pattrn
                       in testGroup
                            "Patterns"
                            [ test
                                "trivial"
                                "(nil nil)"
                                (VSymbol "nil", VSymbol "nil")
                            , test
                                "unit"
                                "(() nil)"
                                (VAtom AUnit, VSymbol "nil")
                            , test
                                "vector"
                                "([()] nil)"
                                (VVector [VAtom AUnit], VSymbol "nil")
                            ]
                    ]
                ]
            ]
        ]
