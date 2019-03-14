module Test.Parser
  ( tree
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Parser
import qualified Parser.Pattern                as Pattern
import qualified Parser.Reference              as Reference
import qualified Syntax.Atom                   as Atom
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Reference              as Reference
import qualified Syntax.Term                   as Term
import           Test.Utils              hiding ( parse )

tester p name inp out = testCase name $ parse p "<test>" inp @?= Right out

tree :: TestTree
tree = testGroup
  "Parser"
  [ testGroup
    "Primitives"
    [ let test = tester Reference.variableName
      in  testGroup
            "Variable names"
            [ test "compact name" "foobar" $ var "foobar"
            , test "qualified name" "foo/bar" $ varIn ["foo"] "bar"
            , test "deeply qualified name" "foo.bar/baz" $ varIn ["foo", "bar"] "baz"
            , test "operator (plus)" "+" $ var "+"
            , test "operator (times)" "*" $ var "*"
            ]
    , let test = tester Reference.moduleName
      in  testGroup
            "Module names"
            [ test "compact name" "foo" $ Reference.Module ["foo"]
            , test "nested name" "foo.bar" $ Reference.Module ["foo", "bar"]
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
          , test
            "recursive function"
            "(recur f (fn [x] (f x)))"
            (Term.Fix noContext
                      (var "f")
                      (lambda "x" (Term.Application noContext (sym "f") [sym "x"]))
            )
          ]
        , testGroup
          "Applications"
          [test "one plus one" "(+ 1 1)" $ Term.Application noContext (sym "+") [int 1, int 1]]
        , testGroup
          "Tuples"
          [ test "single element" "{nil}" $ tuple [(1, unit)]
          , test "two elements" "{nil 1}" $ tuple [(1, unit), (2, int 1)]
          ]
        , testGroup
          "Records"
          [ test "empty" "{}" $ record mempty
          , test "empty w/ space" "{ }" $ record mempty
          , test "single unqualified field" "{:foo nil}" $ record [(kw "foo", unit)]
          , test "single qualified field" "{:foo/bar nil}" $ record [(kwIn ["foo"] "bar", unit)]
          , test "single deeply qualified field" "{:foo.bar/baz nil}"
            $ record [(kwIn ["foo", "bar"] "baz", unit)]
          , test "two unqualified fields"        "{:foo nil :bar 1}"
            $ record [(kw "foo", unit), (kw "bar", int 1)]
          , test "nested record"                 "{:foo {:bar nil}}"
            $ record [(kw "foo", record [(kw "bar", unit)])]
          , test "repeated field" "{:foo nil :foo 1}" $ record [(kw "foo", int 1)]
          ]
        , testGroup
          "Variants"
          [ test "unqualified tag" "[:foo nil]"     (variant (kw "foo") unit)
          , test "qualified tag"   "[:foo/bar nil]" (variant (kwIn ["foo"] "bar") unit)
          , test "deeply qualified tag"
                 "[:foo.bar/baz nil]"
                 (variant (kwIn ["foo", "bar"] "baz") unit)
          , test "nested variant" "[:foo [:bar nil]]" (variant (kw "foo") (variant (kw "bar") unit))
          , test "vector inside variant"
                 "[:foo [1 2]]"
                 (variant (kw "foo") (Term.Vector noContext $ map int [1, 2]))
          ]
        , testGroup
          "Vectors"
          [ test "empty"          "[]"      (vector mempty)
          , test "empty w/ space" "[ ]"     (vector mempty)
          , test "unit element"   "[nil]"   (vector [unit])
          , test "numbers"        "[1 2 3]" (vector $ map int [1, 2, 3])
          ]
        ]
  , testGroup
    "Pattern matches"
    [ testGroup
        "pattern clauses"
        [ let test = tester (removeContext <$> Pattern.expr)
          in
            testGroup
              "Patterns"
              [ test "trivial" "a"   (Pattern.Symbol noContext (Reference.Local "a"))
              , test "unit"    "nil" (Pattern.Atom noContext Atom.Unit)
              , test "unit vector"
                     "[nil]"
                     (Pattern.Vector noContext [Pattern.Atom noContext Atom.Unit])
              ]
        ]
    ]
  ]
