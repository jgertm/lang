(ns lang.code-generator-test
  (:require [lang.test-prelude :refer :all]))

(fact "atoms generate code"
  (with-state-changes
    [(before :facts 
       (run :code-generator
         (defmodule lang.code-generator-test.atom-definitions
           (:skip-implicits))
         (def foo 2)
         (def bar "bar")
         (def baz true)
         #_(def quux nil) ; TODO: implement Unit
         ))]
    (fact
      (eval 'lang.code_generator_test.atom_definitions/foo)
      => 2
      (eval 'lang.code_generator_test.atom_definitions/bar)
      => "bar"
      (eval 'lang.code_generator_test.atom_definitions/baz)
      => true)))

(fact "functions generate code"
  (with-state-changes
    [(before :facts 
       (run :code-generator
         (defmodule lang.code-generator-test.lambda-definitions
           (:skip-implicits))
         (defn id [x] x)
         (defn const [x y] x)))]
    (fact
      (eval '(lang.code_generator_test.lambda_definitions/id 2))
      => 2
      (eval '(lang.code_generator_test.lambda_definitions/const 1 2))
      => 1)))

(fact "typedefs generate code"
  (with-state-changes
    [(before :facts 
       (run :code-generator
         (defmodule lang.code-generator-test.type-definitions
           (:skip-implicits))
         (deftype (Option T)
             (| [:none]
                [:some T]))
         (defn which-is-it [o]
           (match o
             [:none] "just a none."
             [:some _] "it's a some!"))
         (defn default [d o]
           (match o
             [:none] d
             [:some v] v))
         (def none [:none])
         (def some [:some 1])))]
    (fact
      (eval '(lang.code_generator_test.type_definitions/which_is_it
               lang.code_generator_test.type_definitions/none))
      => "just a none."
      (eval '(lang.code_generator_test.type_definitions/which_is_it
               lang.code_generator_test.type_definitions/some))
      => "it's a some!"
      (eval '(lang.code_generator_test.type_definitions/default
               2
               lang.code_generator_test.type_definitions/none))
      => 2
      (eval '(lang.code_generator_test.type_definitions/default
               2
               lang.code_generator_test.type_definitions/some))
      => 1)))
