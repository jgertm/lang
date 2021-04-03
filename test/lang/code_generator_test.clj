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
      (eval '(lang.code_generator_test.type_definitions/which-is-it
               lang.code_generator_test.type_definitions/none))
      => "just a none."
      (eval '(lang.code_generator_test.type_definitions/which-is-it
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

(facts "typeclasses generate code"
  (with-state-changes
    [(before :facts
       (run :code-generator
         (defmodule lang.code-generator-test.typeclasses
           (:skip-implicits))
         (defclass (Veracious T)
           (true? :$ (-> T Bool)))
         (definstance (Veracious Bool)
           (true? [bool] bool))
         (defn is-it-true? [x] (true? x))
         (def nope (is-it-true? false))))]
    (fact
      (eval 'lang.code_generator_test.typeclasses/nope)
      => false)))

(facts "imports generate code"
  (with-state-changes
    [(before :facts
       (run :code-generator
         (defmodule lang.code-generator-test.imports
           (:skip-implicits)
           (:import [lang.io :as io]))
         (defn print [arg]
           (io/println "foofoo"))))]
    (fact
      (eval '(lang.code_generator_test.imports/print 0))
      => nil)))

(facts "recursive types generate code"
  (with-state-changes
    [(before :facts
       (run :code-generator
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
           [:cons {:value 1 :next [:cons {:value 3 :next [:nil]}]}])))])
  (fact 
    (eval `(lang.code_generator_test.list/fold
             lang.math/_PLUS_
             ~(biginteger 0)
             lang.code_generator_test.list/some_list))
    => 4))

(facts "really complicated types generate code"
  (with-state-changes
    [(before :facts
       (run :code-generator
         (defmodule lang.code-generator-test.alist
           (:import [lang.io :as io]))
         (deftype (Alist T)
             (| [:nil]
                [:cons {:current T :next (Alist T)}]))
         (deftype (Entry K V)
             {:key K :value V})
         (deftype (Option T)
             (| [:none]
                [:some T]))
         (defclass (Eq T)
           (= :$ (-> T T Bool)))
         (definstance (Eq Integer)
           (= [a b]
             (. (. a (java.math.BigInteger/equals b))
               (java.lang.Boolean/valueOf))))
         (defn insert [alist key value]
           [:cons {:current {:key key :value value} :next alist}])
         (defn get [alist key]
           (match alist
             [:nil] [:none]
             [:cons {:current {:key k :value v} :next next}]
             (if (= k key)
               [:some v]
               (get next key))))
         (defn retrieve [arg]
           (get (insert (insert (insert [:nil] 1 "one") 2 "two") 3 "three") 2))
         (defn main [arv :$ (Array String)]
           (match (retrieve 1)
             [:some res] (io/println res)
             [:none] (io/println "FAIL")))))])
  (fact 
    (.value (eval `(lang.code_generator_test.alist/retrieve nil)))
    => "two"))
