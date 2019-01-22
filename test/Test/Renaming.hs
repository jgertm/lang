module Test.Renaming
  ( tree
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Error
import qualified Renaming
import qualified Syntax.Common                 as Common
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Term                   as Term
import           Test.Utils

tester outC name inp out =
  let process = map removeContext . Renaming.rename . parse
  in  testCase name $ process inp @?= outC out

test :: TestName -> Text -> Term.Term Test -> TestTree
test = tester Right

testError :: TestName -> Text -> RenamingError -> TestTree
testError = tester (Left . Renaming)

tree :: TestTree
tree = testGroup
  "Renaming"
  [ testError "bare symbol" "a" (UnknownSymbol "a")
  , test "lambda-bound symbol" "(fn [x] x)"        (Term.Lambda noContext (var "x") (sym "x"))
  , test "let-bound symbol"    "(let [[a nil]] a)" (Term.Let noContext [(var "a", unit)] (sym "a"))
  , test "let-bound colliding symbols"
         "(let [[a nil] [a 1]] a)"
         (Term.Let noContext [(var "a", unit), (var "a1", int 1)] (sym "a1"))
  , test
    "let-bound colliding symbols with a lambda"
    "(let [[a 1] [a (fn [x] a)]] (a 2))"
    (Term.Let noContext
              [(var "a", int 1), (var "a1", Term.Lambda noContext (var "x") (sym "a"))]
              (Term.Application noContext (sym "a1") [int 2])
    )
  , test
    "let id apply eval"
    "(let [[foo (fn [x] x)]] (foo 1))"
    (Term.Let noContext
              [(var "foo", Term.Lambda noContext (var "x") (sym "x"))]
              (Term.Application noContext (sym "foo") [int 1])
    )
  , test
    "match stmt"
    "(let [[foo nil]] (match nil (foo foo)))"
    (Term.Let noContext
              [(var "foo", unit)]
              (Term.Match noContext unit [([Pattern.Symbol noContext (var "foo1")], sym "foo1")])
    )
  ]
