module Test.Interpreter
  ( tree
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Interpreter
import qualified Parser
import qualified Renaming
import qualified Syntax.Term                   as Term
import           Test.Utils


test :: TestName -> Text -> Term.Term Test -> TestTree
test name inp out =
  testCase name
    $   second removeContext
               (Interpreter.eval =<< Renaming.rename =<< Parser.parse Parser.term "<test>" inp)
    @?= Right out

tree :: TestTree
tree = testGroup
  "Interpreter"
  [ testGroup
    "Lambda calculus"
    [ test "id eval"     "((fn [x] x) 1)"              (int 1)
    , test "id id eval"  "((fn [x] x) ((fn [x] x) 1))" (int 1)
    , test "const eval"  "((fn [x y] x) 1 2)"          (int 1)
    , test "ignore eval" "((fn [x y] y) 1 2)"          (int 2)
    , test
      "compose eval"
      "(let [[compose (fn [g f] (fn [x] (g (f x))))] [succ (fn [x] (+ 1 x))] [double (fn [x] (* 2 x))] [foo (compose double succ)]] (foo 2))"
      (int 6)
    ]
  , testGroup
    "Conditions"
    [test "basic" "(if true 1 2)" (int 1), test "complicated" "(if ((fn [x] x) false) 1 2)" (int 2)]
  , testGroup
    "Native calls"
    [ test "addition"          "(+ 1 2)"           (int 3)
    , test "minus (binary)"    "(- 2 1)"           (int 1)
    , test "minus (unary)"     "(- 2)"             (int $ -2)
    , test "long addition"     "(+ 1 2 3 4 5 6 7)" (int $ sum [1 .. 7])
    , test "nested arithmetic" "(* 2 (+ 1 3))"     (int 8)
    ]
  , testGroup
    "Pattern matching"
    [ test "match stmt"            "(match 2 (1 1) (2 2))"               (int 2)
    , test "fn + match stmt"       "((fn [x] (match x (0 1) (1 0))) 1)"  (int 0)
    , test "match stmt (capture)"  "(match 1 (0 0) (n (+ n 1)))"         (int 2)
    , test "match stmt (aliasing)" "(let [[foo 0]] (match 1 (foo foo)))" (int 1)
    ]
  , testGroup
    "Let binding"
    [ test "let id apply eval"       "(let [[foo (fn [x] x)]] (foo 1))"         (int 1)
    , test "let id apply apply eval" "(let [[id (fn [x] x)]] (id (id 1)))"      (int 1)
    , test "dependent"               "(let [[a 1] [b ((fn [x] x) a)]] b)"       (int 1)
    , test "rebinding"               "(let [[a nil] [a ((fn [x y] x) a 1)]] a)" unit
    ]
  , testGroup
    "Recursion"
    [ test "simple recursive function "
           "((recur f (fn [x] (match x (3 nil) (n (f (+ 1 n)))))) 1)"
           unit
    , test "factorial" "((recur fac (fn [x] (match x (0 1) (n (* n (fac (- n 1))))))) 4)" (int 24)
    ]
  ]

