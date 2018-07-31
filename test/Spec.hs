module Main where

import           Data.Functor
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
        (runInterpreter mempty . interpretValue =<< parse expr "<test>" inp) @?=
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
            [ let test = tester (void <$> name)
               in testGroup
                    "Lexeme"
                    [ test "compact name" "foobar" $ ESymbol () "foobar"
                    , test "qualified name" "foo.bar" $ ESymbol () "foo.bar"
                    ]
            ]
        , testGroup "Values" $
          let test = tester (void <$> expr)
           in [ testGroup
                  "Lambdas"
                  [ test
                      "id lambda"
                      "(fn [x] x)"
                      (ELambda () "x" (ESymbol () "x"))
                  , test
                      "const lambda"
                      "(fn [x y] x)"
                      (ELambda () "x" (ELambda () "y" (ESymbol () "x")))
                  , test
                      "id application"
                      "((fn [x] x) 1)"
                      (EApplication
                         ()
                         (ELambda () "x" (ESymbol () "x"))
                         (EAtom () (AInteger 1)))
                  ]
              , testGroup
                  "Vectors"
                  [ test "empty" "[]" (EVector () mempty)
                  , test "empty w/ space" "[ ]" (EVector () mempty)
                  , test "unit element" "[()]" (EVector () [EAtom () AUnit])
                  , test
                      "numbers"
                      "[1 2 3]"
                      (EVector () $ fmap (EAtom () . AInteger) [1, 2, 3])
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
        [ test "integer literal" (EAtom () (AInteger 42)) integer
        , test "string literal" (EAtom () (AString "foobar")) string
        , test "boolean literal" (EAtom () (ABoolean True)) boolean
        , test
            "integer vector"
            (EVector () [EAtom () (AInteger 42)])
            (vector integer)
        , testError
            "heterogenous vector"
            (EVector () [EAtom () (AInteger 42), EAtom () AUnit])
            (TypeMismatch integer unit)
        , test
            "let-bound unit"
            (ELet () "foo" (EAtom () AUnit) (ESymbol () "foo"))
            unit
        ]
