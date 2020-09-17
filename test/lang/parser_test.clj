(ns lang.parser-test
  (:require [lang.test-prelude :refer :all]))

(fact "empty module parses"
  (run :parser
    (defmodule lang.parser-test.empty-module))
  => (exactly
       {:ast/definition :module
        :name           {:reference :module :name ["lang" "parser-test" "empty-module"]}
        :skip-implicits nil
        :imports        nil
        :definitions    []}))

(fact "atoms parse"
  (run :parser
    (defmodule lang.parser-test.atom-definitions)
    (def foo 2)
    (def bar "bar")
    (def baz true)
    (def quux nil))
  => (matches
       {:definitions [{:ast/definition :constant
                       :name           {:reference :constant :name "foo"}
                       :body           {:ast/term :atom :atom {:atom :integer :value "2"}}}
                      {:body {:atom {:atom :string :value "bar"}}}
                      {:body {:atom {:atom :boolean :value true}}}
                      {:body {:atom {:atom :unit :value nil}}}]}))

(fact "functions parse"
  (run :parser
    (defmodule lang.parser-test.lambda-definitions)
    (defn id [x] x)
    (defn const [x y] x))
  => (matches
       {:definitions [{:ast/definition :constant
                       :name           {:reference :constant :name "id"}
                       :body
                       {:ast/term  :recur
                        :reference {:reference :constant :name "id"}
                        :body
                        {:ast/term :lambda
                         :argument {:reference :constant :name "x"}
                         :body
                         {:ast/term   :sequence
                          :operations [{:ast/term :symbol
                                        :symbol   {:reference :constant :name "x"}}]}}}}
                      {:body
                       {:ast/term  :recur
                        :reference {:name "const"}
                        :body
                        {:ast/term :lambda
                         :argument {:name "x"}
                         :body
                         {:ast/term :lambda
                          :argument {:name "y"}
                          :body
                          {:operations [{:symbol {:name "x"}}]}}}}}]}))
