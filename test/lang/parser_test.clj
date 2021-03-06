(ns lang.parser-test
  (:require [lang.test-prelude :refer :all]))

(fact "empty module parses"
  (run :parser
    (defmodule lang.parser-test.empty-module))
  => (matches
       {:ast/definition :module
        :name           {:reference :module :name ["lang" "parser-test" "empty-module"]}
        :skip-implicits nil
        :imports        nil
        :definitions    []})

  (run :parser
    (defmodule lang.parser-test.empty-module
      (:skip-implicits)))
  => (matches
       {:ast/definition :module
        :name           {:reference :module :name ["lang" "parser-test" "empty-module"]}
        :skip-implicits true
        :imports        nil
        :definitions    []})

  (run :parser
    (defmodule lang.parser-test.empty-module
      (:import [lang.io :as io]
               [lang.option :as option])))
  => (matches
       {:ast/definition :module
        :name           {:reference :module :name ["lang" "parser-test" "empty-module"]}
        :skip-implicits nil
        :imports        [{:module {:reference :module :name ["lang" "io"]}
                          :alias  {:reference :module :name ["io"]}}
                         {:module {:name ["lang" "option"]}
                          :alias  {:name ["option"]}}]
        :definitions    []}))

(fact "modules can be flat or contained"
  (run :parser
    (defmodule lang.parser-test.flat-module)
    (def foo 1))
  => (matches
       {:ast/definition :module
        :definitions [{:ast/definition :constant}]})

  (run :parser
    (defmodule lang.parser-test.contained-module
      (def foo 1)))
  => (matches
       {:ast/definition :module
        :definitions [{:ast/definition :constant}]}))

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
