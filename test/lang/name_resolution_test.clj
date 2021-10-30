(ns lang.name-resolution-test
  (:require [clojure.test :refer [deftest is testing]]
            [lang.test-prelude :refer :all]
            [matcher-combinators.matchers :refer [equals]]
            matcher-combinators.test))

(deftest definition-name-resolution
  (is (match?
       {:definitions
        [{:ast/definition :constant
          :name           (equals
                           {:ast/reference :constant
                            :name      "foo"
                            :in        {:ast/reference :module
                                        :name      ["lang" "name-resolution-test" "definitions"]}})}]}
       (run :name-resolution
         (defmodule lang.name-resolution-test.definitions)
         (def foo 1)))))

(deftest local-name-resolution
  (is (match?
       {:definitions
        [{:ast/definition :constant
          :body
          {:ast/term :recur
           :body
           {:ast/term :lambda
            :body
            {:ast/term :symbol
             :symbol   (equals {:ast/reference :constant
                                :name      "bar"})}}}}]}
       (run :name-resolution
         (defmodule lang.name-resolution-test.locals)
         (defn foo [bar] bar)))) )

(deftest variable-capture
  (is (match?
       {:definitions
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
             :body      {:ast/term :symbol :symbol {:name "bar"}}}}}}]}
       (run :name-resolution
         (defmodule lang.name-resolution-test.capture)
         (defn foo [foo bar] (fn [baz] bar))))))
