module Test.Parser
  ( tree
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Parser
import qualified Syntax.Atom                   as Atom
import qualified Syntax.Common                 as Common
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Term                   as Term
import           Test.Utils              hiding ( parse )

tester p name inp out = testCase name $ (removeContext <$> parse p "<test>" inp) @?= Right out

tree :: TestTree
tree = testGroup
  "Parser"
  [ testGroup
    "Primitives"
    [ let test = tester (removeContext <$> name)
      in  testGroup
            "Lexeme"
            [ test "compact name" "foobar" $ sym "foobar"
            , test "qualified name" "foo.bar" $ sym "foo.bar"
            ]
    ]
  , testGroup "Values"
    $ let test = tester (removeContext <$> term)
      in
        [ testGroup
          "Lambdas"
          [ test "id lambda" "(fn [x] x)" $ lambda "x" (sym "x")
          , test "const lambda" "(fn [x y] x)" $ lambda "x" (lambda "y" (sym "x"))
          , test "id application"
                 "((fn [x] x) 1)"
                 (Term.Application noContext (lambda "x" (sym "x")) [int 1])
          ]
        , testGroup
          "Vectors"
          [ test "empty"          "[]"      (Term.Vector noContext mempty)
          , test "empty w/ space" "[ ]"     (Term.Vector noContext mempty)
          , test "unit element"   "[nil]"   (Term.Vector noContext [unit])
          , test "numbers"        "[1 2 3]" (Term.Vector noContext $ map int [1, 2, 3])
          ]
        ]
  , testGroup
    "Pattern matches"
    [ testGroup
        "pattern clauses"
        [ let test = tester (removeContext <$> patternExpr)
          in
            testGroup
              "Patterns"
              [ test "trivial" "a"   (Pattern.Symbol noContext (Common.Single "a"))
              , test "unit"    "nil" (Pattern.Atom noContext Atom.Unit)
              , test "unit vector"
                     "[nil]"
                     (Pattern.Vector noContext [Pattern.Atom noContext Atom.Unit])
              ]
        ]
    ]
  ]
