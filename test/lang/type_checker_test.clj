(ns lang.type-checker-test
  (:require [lang.test-prelude :refer :all]
            [lang.module :as module]))

(fact "atoms typecheck"
  (->
    (run :type-checker
      (defmodule lang.type-checker-test.atom-definitions)
      (def foo 2)
      (def bar "bar")
      (def baz true)
      (def quux nil))
    :values)
  => (matches {{:reference :constant :name "foo"}
               [{:ast/type :named
                 :name     {:reference :type
                            :name      "Integer"
                            :in        {:reference :module :name ["lang" "builtin"]}}}
                :principal]
               {:reference :constant :name "bar"}
               [{:name {:name "String"}} :principal]
               {:reference :constant :name "baz"}
               [{:name {:name "Bool"}} :principal]
               {:reference :constant :name "quux"}
               [{:name {:name "Unit"}} :principal]}))


(fact "functions typecheck"
  (->
    (run :type-checker
      (defmodule lang.parser-test.lambda-definitions)
      (defn id [x] x)
      (defn const [x y] x))
    :values)
  => (matches
       {{:reference :constant :name "id"}
        [{:ast/type :forall
          :variable {:ast/type :universal-variable}
          :body {:ast/type :function
                 :domain {:ast/type :universal-variable}
                 :return {:ast/type :universal-variable}}}
         :principal]}))
