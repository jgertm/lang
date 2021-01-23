(ns lang.name-resolution-test
  (:require [lang.test-prelude :refer :all]))

(fact "definition names get resolved to absolute references"
  (run :name-resolution
    (defmodule lang.name-resolution-test.definitions)
    (def foo 1))
  => (matches
       {:definitions
        [{:ast/definition :constant
          :name           (equals
                            {:reference :constant
                             :name      "foo"
                             :in        {:reference :module
                                         :name      ["lang" "name-resolution-test" "definitions"]}})}]}))

(fact "local names remain unqualified"
  (run :name-resolution
    (defmodule lang.name-resolution-test.locals)
    (defn foo [bar] bar))
  => (matches {:definitions
               [{:ast/definition :constant
                 :body
                 {:ast/term :recur
                  :body
                  {:ast/term :lambda
                   :body
                   {:ast/term :symbol
                    :symbol   (equals {:reference :constant
                                       :name      "bar"})}}}}]}) )

(fact "captured variables are recorded"
  (run :name-resolution
    (defmodule lang.name-resolution-test.capture)
    (defn foo [foo bar] (fn [baz] bar)))
  => (matches {:definitions
               [{:ast/definition :constant
                 :body
                 {:ast/term :recur
                  :body
                  {:ast/term  :lambda
                   :arguments [{:name "foo"} {:name "bar"}]
                   :name-resolution/captured-symbols
                   (equals #{})
                   :body
                   {:ast/term  :lambda
                    :arguments [{:name "baz"}]
                    :name-resolution/captured-symbols
                    #{{:name "bar"}}
                    :body      {:ast/term :symbol :symbol {:name "bar"}}}}}}]}))
