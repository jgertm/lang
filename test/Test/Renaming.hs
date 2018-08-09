module Test.Renaming
  ( tree
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Error
import qualified Parser
import           Renaming         (Renaming)
import qualified Renaming
import           Syntax
import           Test.Utils

tester outC name inp out =
  let process = map removeContext . Renaming.rename . parse
   in testCase name $ process inp @?= outC out

test = tester Right

testError = tester (Left . Renaming)

tree =
  testGroup
    "Renaming"
    [ testError "bare symbol" "a" (UnknownSymbol "a")
    , test
        "lambda-bound symbol"
        "(fn [x] x)"
        (ELambda noContext [var "x"] (sym "x"))
    , test
        "let-bound symbol"
        "(let [[a nil]] a)"
        (ELet noContext [(var "a", unit)] (sym "a"))
    , test
        "let-bound colliding symbols"
        "(let [[a nil] [a 1]] a)"
        (ELet noContext [(var "a", unit), (var "a1", int 1)] (sym "a1"))
    , test
        "let-bound colliding symbols with a lambda"
        "(let [[a 1] [a (fn [x] a)]] (a 2))"
        (ELet
           noContext
           [(var "a", int 1), (var "a1", ELambda noContext [var "x"] (sym "a"))]
           (EApplication noContext (sym "a1") [int 2]))
    , test
        "let id apply eval"
        "(let [[foo (fn [x] x)]] (foo 1))"
        (ELet
           noContext
           [(var "foo", ELambda noContext [var "x"] (sym "x"))]
           (EApplication noContext (sym "foo") [int 1]))
    ]
