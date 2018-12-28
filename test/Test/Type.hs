module Test.Type
  ( tree
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Error                          ( TypeError(..) )
import qualified Error
import qualified Test.Utils                    as Utils
import qualified Type
import           Type.Expression               as Type
import           Type.Types

test :: TestName -> Text -> Type -> TestTree
test name source expectedType = testCase name $ case Type.infer $ Utils.parse source of
  Left  err          -> assertFailure (show err)
  Right inferredType -> inferredType @?= expectedType

testError :: TestName -> Text -> TypeError -> TestTree
testError name source expectedError =
  testCase name $ Type.infer (Utils.parse source) @?= Left (Error.Type expectedError)

tree, inference :: TestTree
tree = testGroup "Types" [inference]

inference = testGroup
  "Inference"
  [ test "unit"     "nil"        Type.unit
  , test "integer"  "42"         Type.integer
  , test "boolean"  "true"       Type.boolean
  , test "string"   "\"foobar\"" Type.string
  , test "addition" "(+ 2 3)"    Type.integer
  , test "identity function" "(fn [x] x)"
    $ let var = Var "delta"
      in  Forall var Type (Function (UniversalVariable var) (UniversalVariable var))
  , test "constant function" "(fn [x] nil)"
    $ let var = Var "delta" in Forall var Type (Function (UniversalVariable var) Type.unit)
  , test "successor function" "(fn [n] (+ 1 n))" (Function Type.integer Type.integer)
  , test "compose function"        "(fn [g f] (fn [x] (g (f x))))"       Type.unit -- FIXME
  , test "let binding"             "(let [[foo nil]] foo)"               Type.unit
  , test "complicated let binding" "(let [[foo 1] [bar (+ 1 foo)]] bar)" Type.integer
  , test "if stmt"                 "(if true nil nil)"                   Type.unit
  , test "match stmt"              "(match 1 (2 nil) (1 nil))"           Type.unit
  , test "complicated match stmt"  "(match 1 (2 0) (n (+ n 1)))"         Type.integer
  , testError "addition error"             "(+ 2 nil)"                   (TypeMismatch integer unit)
  , testError "if stmt test error"         "(if nil 1 2)"                (TypeMismatch boolean unit)
  , testError "if stmt branch error"       "(if true 1 nil)"             (TypeMismatch integer unit)
  , testError "match stmt prototype error" "(match 1 (nil nil))"         (TypeMismatch integer unit)
  , testError "match stmt pattern error"   "(match 1 (1 nil) (nil nil))" (TypeMismatch integer unit)
  , testError "match stmt body error"      "(match 1 (1 nil) (2 1))"     (TypeMismatch unit integer)
  ]
