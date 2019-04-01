module Test.Type
  ( tree
  )
where

import qualified Data.Map.Strict               as Map
import           Test.Tasty
import           Test.Tasty.HUnit

import           Application                   as App
import           Error                          ( TypeError(..) )
import qualified Error
import qualified Syntax.Atom                   as Atom
import qualified Syntax.Common                 as Syntax
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Term                   as Term
import qualified Test.Utils                    as Utils
import qualified Type
import qualified Type.Context                  as Ctx
import           Type.Expression               as Type
import qualified Type.Match                    as Match
import           Type.Types


tree, inference :: TestTree
tree = testGroup "Types" [context, inference, patternExpansion]

context =
  let marker = Marker . Utils.tvar
  in  testGroup
        "Context"
        [ testCase "split"
        $   Ctx.split (Context (map marker [1, 2, 3, 4, 5, 6])) (marker 4)
        @?= (Context (map marker [1, 2, 3]), Context (map marker [5, 6]))
        , testCase "drop"
        $   Ctx.drop (Context (map marker [1, 2, 3, 4, 5, 6])) (marker 4)
        @?= Context (map marker [1, 2, 3])
        ]

inference
  = let
      test :: TestName -> Text -> Type -> TestTree
      test name source expectedType =
        testCase name $ case App.run $ Type.infer $ Utils.parse source of
          Left  err          -> assertFailure (show err)
          Right inferredType -> inferredType @?= expectedType
      testError :: TestName -> Text -> TypeError -> TestTree
      testError name source expectedError =
        testCase name $ App.run (Type.infer $ Utils.parse source) @?= Left
          (Error.Type expectedError)
    in
      testGroup
        "Inference"
        [ testGroup
          "Atoms"
          [ test "unit"    "nil"        Type.unit
          , test "integer" "42"         Type.integer
          , test "boolean" "true"       Type.boolean
          , test "string"  "\"foobar\"" Type.string
          ]
        , testGroup
          "Values"
          [ testGroup
            "records (closed)"
            [ test "constant" "{:foo nil :bar 1}"
              $ Record
                  Closed
                  (Map.fromList [(Utils.kw "foo", Type.unit), (Utils.kw "bar", Type.integer)])
            , test "if stmt" "(if true {:foo 1 :bar true} {:foo 2 :bar false})"
              $ Record
                  Closed
                  (Map.fromList [(Utils.kw "foo", Type.integer), (Utils.kw "bar", Type.boolean)])
            , test
              "match stmt (consuming)"
              "(match {:foo 1 :bar true} ({:foo 2} 1) ({:bar false} 2) ({:foo 1 :bar true} 3))"
              Type.integer
            , test "match stmt (producing)" "(match nil (nil {:foo true :bar 2}))"
              $ Record
                  Closed
                  (Map.fromList [(Utils.kw "foo", Type.boolean), (Utils.kw "bar", Type.integer)])
            , test "match stmt (producing)" "(match nil (nil {:foo true :bar 2}))"
              $ Record
                  Closed
                  (Map.fromList [(Utils.kw "foo", Type.boolean), (Utils.kw "bar", Type.integer)])
            , test "function (consuming)"
                   "(fn [x] (match x ({:foo 2 :bar _} nil) ({:foo 1 :bar true} nil)))" -- FIXME: smaller pattern first
              $ Function
                  (Record Closed $ Map.fromList
                    [(Utils.kw "foo", Type.integer), (Utils.kw "bar", Type.boolean)]
                  )
                  Type.unit
            ]
          , testGroup
            "tuples"
            [ test "constant" "{1 nil true}"
              $ Tuple (Map.fromList [(1, Type.integer), (2, Type.unit), (3, Type.boolean)])
            , test "if stmt"  "(if true {true false} {false true})"
              $ Tuple (Map.fromList [(1, Type.boolean), (2, Type.boolean)])
            , test "match stmt (consuming)"
                   "(match {true true} ({false _} 1) ({true true} 2))"
                   Type.integer
            , test "match stmt (producing)" "(match nil (nil {true 2}))"
              $ Tuple (Map.fromList [(1, Type.boolean), (2, Type.integer)])
            , test "function (consuming)"   "(fn [x] (match x ({true 1} nil)))"
              $ Function (Tuple $ Map.fromList [(1, Type.boolean), (2, Type.integer)]) Type.unit
            ]
          , testGroup
            "variants (open)"
            [ test "constant" "#+[:foo 1]"
              $ Variant (Open (Utils.tvar 1)) (Map.singleton (Utils.kw "foo") Type.integer)
            , test "if stmt" "(if true #+[:foo 1] #+[:bar nil])" $ Variant
              (Open (Utils.tvar 1))
              (Map.fromList [(Utils.kw "foo", Type.integer), (Utils.kw "bar", Type.unit)])
            , test "match stmt (consuming)"
                   "(match #+[:foo 1] (#+[:foo 2] true) (#+[:foo 1] false))"
                   Type.boolean
            , test "match stmt (producing)"
                   "(match 2 (1 #+[:foo nil]) (2 #+[:bar true]) (n #+[:quux n]))"
              $ Variant
                  (Open (Utils.tvar 1))
                  (Map.fromList
                    [ (Utils.kw "foo" , Type.unit)
                    , (Utils.kw "bar" , Type.boolean)
                    , (Utils.kw "quux", Type.integer)
                    ]
                  )
            , test "function (consuming)" "(fn [x] (match x (#+[:foo nil] 1) (#+[:bar true] 2)))"
              $ Function
                  (Variant
                    (Open (Utils.tvar 2))
                    (Map.fromList [(Utils.kw "foo", Type.unit), (Utils.kw "bar", Type.boolean)])
                  )
                  Type.integer
            ]
          ]
        , testGroup
          "Builtins"
          [ test "addition" "(+ 2 3)" Type.integer
          , testError "addition error" "(+ 2 nil)" (TypeMismatch integer unit)
          ]
        , testGroup "Partial application"
                    [test "successor function" "(+ 1)" (Function Type.integer Type.integer)]
        , testGroup
          "Higher order functions"
          [ test "identity function" "(fn [x] x)"
            $ let var = Utils.tvar 4
              in  Forall var Type (Function (UniversalVariable var) (UniversalVariable var))
          , test "constant function" "(fn [x] nil)"
            $ let var = Utils.tvar 4
              in  Forall var Type (Function (UniversalVariable var) Type.unit)
          , test "successor function" "(fn [n] (+ 1 n))" (Function Type.integer Type.integer)
          , test "delayed arguent"  "(fn [f] (f nil))"
            $ let var = Utils.tvar 6
              in  Forall
                    var
                    Type
                    (Function (Function Type.unit (UniversalVariable var)) (UniversalVariable var))
          , test "apply function"   "(fn [f x] (fn [x] (f x)))"
            $ let
                alpha = Utils.tvar 12
                beta  = Utils.tvar 11
                gamma = Utils.tvar 10
              in
                Forall
                  alpha
                  Type
                  (Forall
                    beta
                    Type
                    (Function
                      (Function (UniversalVariable alpha) (UniversalVariable beta))
                      (Forall
                        gamma
                        Type
                        (Function (UniversalVariable gamma)
                                  (Function (UniversalVariable alpha) (UniversalVariable beta))
                        )
                      )
                    )
                  )
          , test "compose function" "(fn [g f] (fn [x] (g (f x))))"
            $ let alpha = Utils.tvar 12
                  beta  = Utils.tvar 14
                  gamma = Utils.tvar 13
              in  Forall
                    beta
                    Type
                    (Forall
                      gamma
                      Type
                      (Function
                        (Function (UniversalVariable beta) (UniversalVariable gamma))
                        (Forall
                          alpha
                          Type
                          (Function (Function (UniversalVariable alpha) (UniversalVariable beta))
                                    (Function (UniversalVariable alpha) (UniversalVariable gamma))
                          )
                        )
                      )
                    )
          , test "composed functions"
                 "((fn [g f] (fn [x] (g (f x)))) (fn [n] (+ 1 n)) (fn [m] (* 2 m)))"
                 (Function Type.integer Type.integer)
          ]
        , testGroup
          "Control flow"
          [ testGroup
            "let binding"
            [ test "simple"      "(let [[foo nil]] foo)"               Type.unit
            , test "complicated" "(let [[foo 1] [bar (+ 1 foo)]] bar)" Type.integer
            ]
          , testGroup
            "if stmt"
            [ test "simple"      "(if true nil nil)" Type.unit
            , test "complicated" "(if false 1 2)"    Type.integer
            , testError "if stmt test error"   "(if nil 1 2)"    (TypeMismatch boolean unit)
            , testError "if stmt branch error" "(if true 1 nil)" (TypeMismatch integer unit)
            ]
          , testGroup
            "pattern matching"
            [ test "simple"       "(match 1 (2 nil) (1 nil))"       Type.unit
            , test "alternatives" "(match 1 ((| 0 1) nil) (2 nil))" Type.unit
            , test "complicated"  "(match 1 (2 0) (n (+ n 1)))"     Type.integer
            , test "function" "(fn [x] (match x (1 nil)))" (Function Type.integer Type.unit)
            , test "function (alternatives)"
                   "(fn [x] (match x ((| 0 1) nil)))"
                   (Function Type.integer Type.unit)
            , testError "prototype error" "(match 1 (nil nil))" (TypeMismatch integer unit)
            , testError "pattern error" "(match 1 (1 nil) (nil nil))" (TypeMismatch integer unit)
            , testError "body error" "(match 1 (1 nil) (2 1))" (TypeMismatch unit integer)
            , testError "alternative error" "(match 1 ((| 1 nil) nil))" (TypeMismatch integer unit)
            ]
          , test "pointless recursion (atom)" "(recur foo 1)" Type.integer
          , test "pointless recursion (successor function)"
                 "(recur foo (fn [x] (+ 1 x)))"
                 (Function Type.integer Type.integer)
          , test "recursive function (factorial)"
                 "(recur fac (fn [x] (match x (0 1) (n (* n (fac (- n 1)))))))"
                 (Function Type.integer Type.integer)
          ]
        ]

patternExpansion = testGroup
  "Pattern expansion"
  [ testGroup
      "variants"
      [ testCase "empty" $ Match.expandVariant [] @?= mempty
      , testCase "single branch"
      $   Match.expandVariant
            [ Term.Branch
                { patterns = [ Pattern.Variant ()
                                               Syntax.Closed
                                               (Utils.kw "foo")
                                               (Pattern.Atom () Atom.Unit)
                             ]
                , body     = Term.Atom () Atom.Unit
                }
            ]
      @?= Map.singleton
            (Utils.kw "foo")
            [Term.Branch {patterns = [Pattern.Atom () Atom.Unit], body = Term.Atom () Atom.Unit}]
      , testCase "multiple branches"
      $   Match.expandVariant
            [ Term.Branch
              { patterns = [ Pattern.Variant ()
                                             Syntax.Closed
                                             (Utils.kw "foo")
                                             (Pattern.Atom () Atom.Unit)
                           ]
              , body     = Term.Atom () Atom.Unit
              }
            , Term.Branch
              { patterns = [Pattern.Variant () Syntax.Closed (Utils.kw "bar") (Pattern.Wildcard ())]
              , body     = Term.Atom () Atom.Unit
              }
            ]
      @?= Map.fromList
            [ ( Utils.kw "foo"
              , [ Term.Branch
                    { patterns = [Pattern.Atom () Atom.Unit]
                    , body     = Term.Atom () Atom.Unit
                    }
                ]
              )
            , ( Utils.kw "bar"
              , [Term.Branch {patterns = [Pattern.Wildcard ()], body = Term.Atom () Atom.Unit}]
              )
            ]
      ]
  ]
