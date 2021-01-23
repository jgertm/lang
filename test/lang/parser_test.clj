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
      (:skip-implicits)
      (:import [lang.io :as io]
               [lang.option :as option])))
  => (matches
       {:ast/definition :module
        :name           {:reference :module :name ["lang" "parser-test" "empty-module"]}
        :skip-implicits true
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
    (defn const [x y] x)
    (defn fx! [x] (println x) x))
  => (matches
       {:definitions [{:ast/definition :constant
                       :name           {:reference :constant :name "id"}
                       :body
                       {:ast/term  :recur
                        :reference {:reference :constant :name "id"}
                        :body
                        {:ast/term :lambda
                         :arguments [{:reference :constant :name "x"}]
                         :body
                         {:ast/term :symbol
                          :symbol   {:reference :constant :name "x"}}}}}
                      {:body
                       {:ast/term  :recur
                        :reference {:name "const"}
                        :body
                        {:ast/term :lambda
                         :arguments [{:name "x"} {:name "y"}]
                         :body
                         {:symbol {:name "x"}}}}}

                      {:body 
                       {:ast/term  :recur
                        :reference {:name "fx!"}
                        :body
                        {:ast/term :lambda
                         :body
                         {:ast/term :sequence
                          :operations
                          [{:ast/term :application}
                           {:ast/term :symbol}]}}}}]}))

(fact "typeclasses parse"
  (run :parser
    (defmodule lang.desugar.typeclasses-test.definitions
      (:skip-implicits))
    (defclass (Veracious T)
      (true? :$ (-> T Bool)))
    (definstance (Veracious Bool)
      (true? [bool] bool)))
  => (matches
       {:definitions
        [{:ast/definition :typeclass
          :name           {:reference :typeclass
                           :name      "Veracious"}
          :params
          [{:reference :type :name "T"}]
          :fields
          {{:reference :constant :name "true?"}
           {:ast/type :function
            :domain   {:ast/type :named
                       :name     {:reference :type :name "T"}}
            :return   {:ast/type :named
                       :name     {:reference :type :name "Bool"}}}}}
         {:ast/definition :typeclass-instance
          :name           {:reference :typeclass :name "Veracious"}
          :types
          [{:ast/type :named :name {:reference :type :name "Bool"}}]
          :superclasses   nil
          :fields
          {{:reference :constant :name "true?"}
           {:ast/term :lambda
            :arguments [{:reference :constant :name "bool"}]
            :body
            {:ast/term :symbol
             :symbol
             {:reference :constant :name "bool"}}}}}]}))
