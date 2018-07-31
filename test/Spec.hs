module Main where

import           Data.Monoid
import           Data.Text        (Text)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Error
import           Inference
import           Interpreter
import           Parser
import           Parser.Abstract  hiding (parser)
import           Syntax.Abstract
import           Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [interpreter, parser, typeInference]

interpreter, parser, typeInference :: TestTree
interpreter =
  let test name inp out =
        testCase name $
        (runInterpreter mempty . interpretValue =<< parse valueExpr "<test>" inp) @?=
        Right out
   in testGroup
        "Interpreter"
        [ test "id eval" "((fn [x] x) 1)" (AInteger 1)
        , test "const eval" "((fn [x y] x) 1 2)" (AInteger 1)
        , test "ignore eval" "((fn [x y] y) 1 2)" (AInteger 2)
        , test
            "let id apply eval"
            "(let [[foo (fn [x] x)]] (foo 1))"
            (AInteger 1)
        ]

parser =
  let tester p name inp out = testCase name $ parse p "<test>" inp @?= Right out
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
        , testGroup "Values" $
          let test = tester valueExpr
           in [ testGroup
                  "Lambdas"
                  [ test "id lambda" "(fn [x] x)" (VLambda "x" (VSymbol "x"))
                  , test
                      "const lambda"
                      "(fn [x y] x)"
                      (VLambda "x" (VLambda "y" (VSymbol "x")))
                  , test
                      "id application"
                      "((fn [x] x) 1)"
                      (VApplication
                         (VLambda "x" (VSymbol "x"))
                         (VAtom (AInteger 1)))
                  ]
              , testGroup
                  "Vectors"
                  [ test "empty" "[]" (VVector mempty)
                  , test "empty w/ space" "[ ]" (VVector mempty)
                  , test "unit element" "[()]" (VVector [VAtom AUnit])
                  , test
                      "numbers"
                      "[1 2 3]"
                      (VVector $ fmap (VAtom . AInteger) [1, 2, 3])
                  ]
              ]
        , testGroup
            "Pattern matches"
            [ testGroup
                "pattern clauses"
                [ let test = tester patternExpr
                   in testGroup
                        "Patterns"
                        [ test "trivial" "nil" (PSymbol "nil")
                        , test "unit" "()" (PAtom AUnit)
                        , test "unit vector" "[()]" (PVector [PAtom AUnit])
                        ]
                ]
            ]
        ]

typeInference =
  let test name inp out =
        testCase name $ runInfer (inferValue inp) @?= Right out
      testError name inp err =
        testCase name $ runInfer (inferValue inp) @?= Left (Inference err)
   in testGroup
        "Type Inference"
        [ test "integer literal" (VAtom (AInteger 42)) integer
        , test "string literal" (VAtom (AString "foobar")) string
        , test "boolean literal" (VAtom (ABoolean True)) boolean
        , test "integer vector" (VVector [VAtom (AInteger 42)]) (vector integer)
        , testError
            "heterogenous vector"
            (VVector [VAtom (AInteger 42), VAtom AUnit])
            (TypeMismatch integer unit)
        , test "let-bound unit" (VLet "foo" (VAtom (AUnit)) (VSymbol "foo")) unit
        ]
