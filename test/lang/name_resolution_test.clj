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
