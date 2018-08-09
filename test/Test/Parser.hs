module Test.Parser
  ( tree
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Parser
import           Syntax           (Atom (..), Binding (..), Context (..),
                                   Pattern' (..), Term' (..))
import           Test.Utils       hiding (parse)

tester p name inp out =
  testCase name $ (removeContext <$> parse p "<test>" inp) @?= Right out

tree :: TestTree
tree =
  testGroup
    "Parser"
    [ testGroup
        "Primitives"
        [ let test = tester (removeContext <$> name)
           in testGroup
                "Lexeme"
                [ test "compact name" "foobar" $
                  ESymbol noContext (Single "foobar")
                , test "qualified name" "foo.bar" $
                  ESymbol noContext (Single "foo.bar")
                ]
        ]
    , testGroup "Values" $
      let test = tester (removeContext <$> term)
       in [ testGroup
              "Lambdas"
              [ test
                  "id lambda"
                  "(fn [x] x)"
                  (ELambda
                     noContext
                     [Single "x"]
                     (ESymbol noContext (Single "x")))
              , test
                  "const lambda"
                  "(fn [x y] x)"
                  (ELambda
                     noContext
                     [Single "x", Single "y"]
                     (ESymbol noContext (Single "x")))
              , test
                  "id application"
                  "((fn [x] x) 1)"
                  (EApplication
                     noContext
                     (ELambda
                        noContext
                        [Single "x"]
                        (ESymbol noContext (Single "x")))
                     [int 1])
              ]
          , testGroup
              "Vectors"
              [ test "empty" "[]" (EVector noContext mempty)
              , test "empty w/ space" "[ ]" (EVector noContext mempty)
              , test "unit element" "[nil]" (EVector noContext [unit])
              , test "numbers" "[1 2 3]" (EVector noContext $ map int [1, 2, 3])
              ]
          ]
    , testGroup
        "Pattern matches"
        [ testGroup
            "pattern clauses"
            [ let test = tester (removeContext <$> patternExpr)
               in testGroup
                    "Patterns"
                    [ test "trivial" "a" (PSymbol noContext (Single "a"))
                    , test "unit" "nil" (PAtom noContext AUnit)
                    , test
                        "unit vector"
                        "[nil]"
                        (PVector noContext [PAtom noContext AUnit])
                    ]
            ]
        ]
    ]
