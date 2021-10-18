(ns lang.code-generator-test
  (:require [clojure.test :refer [deftest testing is]]
            [lang.test-prelude :refer :all]
            matcher-combinators.test))

(deftest atom-codegen
  (do (run :code-generator
        (defmodule lang.code-generator-test.atom-definitions
          (:skip-implicits))
        (def foo 2)
        (def bar "bar")
        (def baz true)
        #_(def quux nil) ; TODO: implement Unit
        )
      (is (= 2 (eval 'lang.code_generator_test.atom_definitions/foo)))
      (is (= "bar" (eval 'lang.code_generator_test.atom_definitions/bar)))
      (is (= true (eval 'lang.code_generator_test.atom_definitions/baz)))))

(deftest function-codegen
  (do
    (run :code-generator (defmodule lang.code-generator-test.lambda-definitions
                           (:skip-implicits))
      (defn id [x] x)
      (defn const [x y] x))
    (is (= 2 (eval '(lang.code_generator_test.lambda_definitions/id 2))))
    (is (= 1 (eval '(lang.code_generator_test.lambda_definitions/const 1 2))))))

(deftest typedef-codegen
  (do (run :code-generator
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
        (def some [:some 1]))
      (is (= "just a none."
             (eval '(lang.code_generator_test.type_definitions/which-is-it
                     lang.code_generator_test.type_definitions/none))))
      (is (= "it's a some!"
             (eval '(lang.code_generator_test.type_definitions/which-is-it
                     lang.code_generator_test.type_definitions/some))))
      (is  (= 2
              (eval '(lang.code_generator_test.type_definitions/default
                      2
                      lang.code_generator_test.type_definitions/none))))
      (is (= 1
             (eval '(lang.code_generator_test.type_definitions/default
                     2
                     lang.code_generator_test.type_definitions/some))))))

(deftest typeclass-codegen
  (do (run :code-generator
        (defmodule lang.code-generator-test.typeclasses
          (:skip-implicits))
        (defclass (Veracious T)
          (true? :$ (-> T Bool)))
        (definstance (Veracious Bool)
          (true? [bool] bool))
        (defn is-it-true? [x] (true? x))
        (def nope (is-it-true? false)))
      (is (= false (eval 'lang.code_generator_test.typeclasses/nope)))))

(deftest import-codegen
  (do (run :code-generator
        (defmodule lang.code-generator-test.imports
          (:skip-implicits)
          (:import [lang.io :as io]))
        (defn print [arg]
          (io/println "foofoo")))
      (is (= nil (eval '(lang.code_generator_test.imports/print 0))))))

(deftest recursive-typedef-codegen
  (do (run :code-generator
        (defmodule lang.code-generator-test.list)
        (deftype (List T)
            (| [:nil]
               [:cons {:value T :next (List T)}]))
        (defn fold
          [op init list]
          (match list
                 [:nil] init
                 [:cons {:value n :next tl}] (op (fold op init tl) n)))
        (def some-list
          [:cons {:value 1 :next [:cons {:value 3 :next [:nil]}]}]))
      (is (= 4
             (eval `(lang.code_generator_test.list/fold
                     lang.math/_PLUS_
                     ~(biginteger 0)
                     lang.code_generator_test.list/some_list))))))

(deftest nested-typedef-codegen
       (do (run :code-generator
             (defmodule lang.code-generator-test.alist
               (:import [lang.io :as io]
                        [lang.option :as option]))
             (deftype (Alist T)
                 (| [:nil]
                    [:cons {:current T :next (Alist T)}]))
             (deftype (Entry K V)
                 {:key K :value V})
             #_(deftype (Option T)
                 (| [:none]
                    [:some T]))
             (defn insert [alist key value]
               [:cons {:current {:key key :value value} :next alist}])
             (defn get [alist key]
               (match alist
                      [:nil] [:option/none]
                      [:cons {:current {:key k :value v} :next next}]
                      (match (= k key)
                        true [:option/some v]
                        false (get next key))))
             (defn retrieve [arg]
               (get (insert (insert (insert [:nil] 1 "one") 2 "two") 3 "three") 2))
             (defn retrieve2 [arg]
               (get (insert (insert [:nil] [:option/some 1] "one") [:option/some 2] "two") [:option/some 1])))

           (is (= "two" (.value (eval `(lang.code_generator_test.alist/retrieve nil)))))
           (is (= "one" (.value (eval `(lang.code_generator_test.alist/retrieve2 nil)))))))
