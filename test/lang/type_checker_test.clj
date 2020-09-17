(ns lang.type-checker-test
  (:require [lang.test-prelude :refer :all]
            [lang.module :as module]))

(fact "atoms typecheck"
  (->
    (run :type-checker
      (defmodule lang.type-checker-test.atom-definitions
        (:skip-implicits))
      (def foo 2)
      (def bar "bar")
      (def baz true)
      (def quux nil))
    :values)
  => (matches
       {{:reference :constant
         :name      "foo"
         :in        {:reference :module
                     :name      ["lang" "type-checker-test" "atom-definitions"]}}
        [{:ast/type :named
          :name     {:reference :type
                     :name      "Integer"
                     :in        {:reference :module :name ["lang" "builtin"]}}}
         :principal]
        {:reference :constant
         :name      "bar"
         :in        {:reference :module
                     :name      ["lang" "type-checker-test" "atom-definitions"]}}
        [{:name {:name "String"}} :principal]
        {:reference :constant
         :name      "baz"
         :in        {:reference :module
                     :name      ["lang" "type-checker-test" "atom-definitions"]}}
        [{:name {:name "Bool"}} :principal]
        {:reference :constant
         :name      "quux"
         :in        {:reference :module
                     :name      ["lang" "type-checker-test" "atom-definitions"]}}
        [{:name {:name "Unit"}} :principal]}))


(fact "functions typecheck"
  (->
    (run :type-checker
      (defmodule lang.parser-test.lambda-definitions
        (:skip-implicits))
      (defn id [x] x)
      (defn const [x y] x))
    :values)
  => (matches
       {{:reference :constant
         :name      "id"
         :in        {:reference :module
                     :name      ["lang" "parser-test" "lambda-definitions"]}}
        [{:ast/type :forall
          :variable {:ast/type :universal-variable}
          :body     {:ast/type :function
                     :domain   {:ast/type :universal-variable}
                     :return   {:ast/type :universal-variable}}}
         :principal]
        {:reference :constant
         :name      "const"
         :in        {:reference :module
                     :name      ["lang" "parser-test" "lambda-definitions"]}}
        [{:ast/type :forall
          :variable {:ast/type :universal-variable}
          :body     {:ast/type :forall
                     :variable {:ast/type :universal-variable}
                     :body     {:ast/type :function
                                :domain   {:ast/type :universal-variable}
                                :return   {:ast/type :function
                                           :domain   {:ast/type :universal-variable}
                                           :return   {:ast/type :universal-variable}}}}}
         :principal]}))
