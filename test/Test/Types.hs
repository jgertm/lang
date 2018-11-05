module Test.Types
  ( tree
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit
import           System.Console.ANSI

import           Error                          ( Error(Inference)
                                                , InferenceError(..)
                                                )
import           Inference
import qualified Syntax
import           Test.Utils
import           Types                          ( PrimitiveType(..)
                                                , Type(..)
                                                )

test :: TestName -> Text -> Type -> TestTree
test name source expectedType = testCase name $ case infer $ parse source of
  Left err -> assertFailure (show err)
  Right (inferredAST, _) ->
    let typ = Syntax.context inferredAST in typ @?= expectedType

testError :: TestName -> Text -> InferenceError -> TestTree
testError name source expectedError =
  testCase name $ infer (parse source) @?= Left (Inference expectedError)

tree, inference :: TestTree
tree = testGroup "Types" [inference]

inference = testGroup
  "Type inference"
  [ test "unit"     "nil"        (Primitive Unit)
  , test "integer"  "42"         (Primitive Integer)
  , test "boolean"  "true"       (Primitive Boolean)
  , test "string"   "\"foobar\"" (Primitive String)
  , test "addition" "(+ 2 3)"    (Primitive Integer)
  , test "identity function"
         "(fn [x] x)"
         (Forall ["a"] (Application "fn" [Variable "a", Variable "a"]))
  , test "constant function"
         "(fn [x] nil)"
         (Forall ["a"] (Application "fn" [Variable "a", Primitive Unit]))
  , test "successor function"
         "(fn [n] (+ 1 n))"
         (Application "fn" [Primitive Integer, Primitive Integer])
  , test "let binding" "(let [[foo nil]] foo)" (Primitive Unit)
  , test "complicated let binding"
         "(let [[foo 1] [bar (+ 1 foo)]] bar)"
         (Primitive Integer)
  , test "if stmt"    "(if true nil nil)"         (Primitive Unit)
  , test "match stmt" "(match 1 (2 nil) (1 nil))" (Primitive Unit)
  , test "complicated match stmt"
         "(match 1 (2 0) (n (+ n 1)))"
         (Primitive Integer)
  , testError "addition error"
              "(+ 2 nil)"
              (UnificationFailure (Primitive Unit) (Primitive Integer))
  , testError "if stmt test error"
              "(if nil 1 2)"
              (UnificationFailure (Primitive Unit) (Primitive Boolean))
  , testError "if stmt branch error"
              "(if true 1 nil)"
              (UnificationFailure (Primitive Unit) (Primitive Integer))
  , testError "match stmt prototype error"
              "(match 1 (nil nil))"
              (UnificationFailure (Primitive Unit) (Primitive Integer))
  , testError "match stmt pattern error"
              "(match 1 (1 nil) (nil nil))"
              (UnificationFailure (Primitive Unit) (Primitive Integer))
  , testError "match stmt body error"
              "(match 1 (1 nil) (2 1))"
              (UnificationFailure (Primitive Integer) (Primitive Unit))
  ]
