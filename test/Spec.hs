module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Error
import           Inference
import           Interpreter
import           Parser
import           Renaming
import           Syntax
import           Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [renaming, interpreter, parser, typeInference]

renaming, interpreter, parser, typeInference :: TestTree
renaming =
  let tester outC name inp out =
        testCase name $
        (rename =<< parse (void <$> expr) "<test>" inp) @?= outC out
      test = tester Right
      testError = tester (Left . Renaming)
   in testGroup
        "Renaming"
        [ testError "bare symbol" "a" (UnknownSymbol "a")
        , test
            "lambda-bound symbol"
            "(fn [x] x)"
            (ELambda noMeta "x0" (ESymbol noMeta "x0"))
        , test
            "let-bound symbol"
            "(let [[a nil]] a)"
            (ELet noMeta "a0" (EAtom noMeta AUnit) (ESymbol noMeta "a0"))
        , test
            "let-bound colliding symbols"
            "(let [[a nil] [a 1]] a)"
            (ELet
               noMeta
               "a0"
               (EAtom noMeta AUnit)
               (ELet
                  noMeta
                  "a1"
                  (EAtom noMeta (AInteger 1))
                  (ESymbol noMeta "a1")))
        , test
            "let id apply eval"
            "(let [[foo (fn [x] x)]] (foo 1))"
            (ELet
               noMeta
               "foo0"
               (ELambda noMeta "x0" (ESymbol noMeta "x0"))
               (EApplication
                  noMeta
                  (ESymbol noMeta "foo0")
                  (EAtom noMeta (AInteger 1))))
        ]

interpreter =
  let test name inp out =
        testCase name $
        (interpret =<< rename =<< parse expr "<test>" inp) @?= Right out
   in testGroup
        "Interpreter"
        [ test "id eval" "((fn [x] x) 1)" (AInteger 1)
        , test "const eval" "((fn [x y] x) 1 2)" (AInteger 1)
        , test "ignore eval" "((fn [x y] y) 1 2)" (AInteger 2)
        , test
            "let id apply eval"
            "(let [[foo (fn [x] x)]] (foo 1))"
            (AInteger 1)
        , test "complicated" "(let [[a nil] [a ((fn [x y] x) a 1)]] a)" AUnit
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
                    [ test "compact name" "foobar" $ ESymbol noMeta "foobar"
                    , test "qualified name" "foo.bar" $ ESymbol noMeta "foo.bar"
                    ]
            ]
        , testGroup "Values" $
          let test = tester (void <$> expr)
           in [ testGroup
                  "Lambdas"
                  [ test
                      "id lambda"
                      "(fn [x] x)"
                      (ELambda noMeta "x" (ESymbol noMeta "x"))
                  , test
                      "const lambda"
                      "(fn [x y] x)"
                      (ELambda
                         noMeta
                         "x"
                         (ELambda noMeta "y" (ESymbol noMeta "x")))
                  , test
                      "id application"
                      "((fn [x] x) 1)"
                      (EApplication
                         noMeta
                         (ELambda noMeta "x" (ESymbol noMeta "x"))
                         (EAtom noMeta (AInteger 1)))
                  ]
              , testGroup
                  "Vectors"
                  [ test "empty" "[]" (EVector noMeta mempty)
                  , test "empty w/ space" "[ ]" (EVector noMeta mempty)
                  , test
                      "unit element"
                      "[nil]"
                      (EVector noMeta [EAtom noMeta AUnit])
                  , test
                      "numbers"
                      "[1 2 3]"
                      (EVector noMeta $ fmap (EAtom noMeta . AInteger) [1, 2, 3])
                  ]
              ]
        , testGroup
            "Pattern matches"
            [ testGroup
                "pattern clauses"
                [ let test = tester patternExpr
                   in testGroup
                        "Patterns"
                        [ test "trivial" "a" (PSymbol "a")
                        , test "unit" "nil" (PAtom AUnit)
                        , test "unit vector" "[nil]" (PVector [PAtom AUnit])
                        ]
                ]
            ]
        ]

typeInference =
  let tester outC name inp out =
        testCase name $ runInfer (inferValue inp) @?= outC out
      test = tester Right
      testError = tester (Left . Inference)
   in testGroup
        "Type Inference"
        [ test "integer literal" (EAtom noMeta (AInteger 42)) integer
        , test "string literal" (EAtom noMeta (AString "foobar")) string
        , test "boolean literal" (EAtom noMeta (ABoolean True)) boolean
        , test
            "integer vector"
            (EVector noMeta [EAtom noMeta (AInteger 42)])
            (vector integer)
        , testError
            "heterogenous vector"
            (EVector noMeta [EAtom noMeta (AInteger 42), EAtom noMeta AUnit])
            (TypeMismatch integer unit)
        , test
            "let-bound unit"
            (ELet noMeta "foo" (EAtom noMeta AUnit) (ESymbol noMeta "foo"))
            unit
        ]

noMeta = [()]
