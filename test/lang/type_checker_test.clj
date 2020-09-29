(ns lang.type-checker-test
  (:require [lang.test-prelude :refer :all]
            [lang.module :as module]))

(facts "atoms typecheck"
  (let [{:strs [foo bar baz quux]}
        (->>
          (run :type-checker
            (defmodule lang.type-checker-test.atom-definitions
              (:skip-implicits))
            (def foo 2)
            (def bar "bar")
            (def baz true)
            (def quux nil))
          :values
          (map (fn [[k v]] [(:name k) v]))
          (into {}))]

    (fact "foo" foo =>
      (matches [{:ast/type :named
                 :name     {:reference :type
                            :name      "Integer"
                            :in        {:reference :module :name ["lang" "builtin"]}}}
                :principal]))

    (fact "bar" bar =>
      (matches [{:name {:name "String"}} :principal]))

    (fact "baz" baz =>
      (matches [{:name {:name "Bool"}} :principal]))

    (fact "quux" quux =>
      (matches [{:name {:name "Unit"}} :principal]))))


(facts "functions typecheck"
  (let [{:strs [id const]}
        (->>
          (run :type-checker
            (defmodule lang.parser-test.lambda-definitions
              (:skip-implicits))
            (defn id [x] x)
            (defn const [x y] x))
          :values
          (map (fn [[k v]] [(:name k) v]))
          (into {}))]

    (fact "id" id =>
      (matches [{:ast/type :forall
                 :variable {:ast/type :universal-variable}
                 :body     {:ast/type :function
                            :domain   {:ast/type :universal-variable}
                            :return   {:ast/type :universal-variable}}}
                :principal]))

    (fact "const" const =>
      (matches [{:ast/type :forall
                 :variable {:ast/type :universal-variable}
                 :body     {:ast/type :forall
                            :variable {:ast/type :universal-variable}
                            :body     {:ast/type :function
                                       :domain   {:ast/type :universal-variable}
                                       :return   {:ast/type :function
                                                  :domain   {:ast/type :universal-variable}
                                                  :return   {:ast/type :universal-variable}}}}}
                :principal]))))
