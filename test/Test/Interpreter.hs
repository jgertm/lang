module Test.Interpreter
  ( tree
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Interpreter
import qualified Parser
import qualified Renaming
import           Syntax
import           Test.Utils

test :: TestName -> Text -> Term' Test -> TestTree
test name inp out =
  testCase name $
  second
    removeContext
    (interpret =<< Renaming.rename =<< Parser.parse Parser.term "<test>" inp) @?=
  Right out

tree :: TestTree
tree =
  testGroup
    "Interpreter"
    [ test "id eval" "((fn [x] x) 1)" (int 1)
    , test "const eval" "((fn [x y] x) 1 2)" (int 1)
    , test "ignore eval" "((fn [x y] y) 1 2)" (int 2)
    , test "let id apply eval" "(let [[foo (fn [x] x)]] (foo 1))" (int 1)
    , test "complicated" "(let [[a nil] [a ((fn [x y] x) a 1)]] a)" unit
    , test "native (addition)" "(+ 1 2)" (int 3)
    , test "if stmt" "(if true 1 2)" (int 1)
    , test "match stmt" "(match 2 (1 1) (2 2))" (int 2)
    ]
