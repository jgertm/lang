(ns lang.type-checker-test
  (:require [clojure.test :refer [deftest is testing]]
            [lang.module :as module]
            [lang.test-prelude :refer :all]
            matcher-combinators.test))

(deftest typecheck-atoms
  (let [module
        (run :type-checker
          (defmodule lang.type-checker-test.atom-definitions
            (:skip-implicits))
          (def foo 2)
          (def bar "bar")
          (def baz true)
          (def quux nil))
        {:strs [foo bar baz quux]}
        (->> module
          :values
          (map (fn [[k v]] [(:name k) v]))
          (into {}))]

    (is (match?
         [{:ast/type :named
                 :name     {:reference :type
                            :name      "Integer"
                            :in        {:reference :module :name ["lang" "builtin"]}}}
          :principal]
         foo))

    (is (match?
         [{:ast/type :named :name {:name "String"}} :principal]
         bar))

    (is (match?
         [{:ast/type :named :name {:name "Bool"}} :principal]
         baz))

    (is (match?
         [{:ast/type :named :name {:name "Unit"}} :principal]
         quux))))

(deftest typecheck-functions
  (let [{:strs [id const]}
        (->>
          (run :type-checker
            (defmodule lang.type-checker-test.lambda-definitions
              (:skip-implicits))
            (defn id [x] x)
            (defn const [x y] x))
          :values
          (map (fn [[k v]] [(:name k) v]))
          (into {}))]

    (is (match?
         [{:ast/type :forall
           :variable {:ast/type :universal-variable}
           :body     {:ast/type :function
                      :domain   {:ast/type :universal-variable}
                      :return   {:ast/type :universal-variable}}}
          :principal]
         id))

    (is (match?
         [{:ast/type :forall
                  :variable {:ast/type :universal-variable}
                  :body     {:ast/type :forall
                             :variable {:ast/type :universal-variable}
                             :body     {:ast/type :function
                                        :domain   {:ast/type :universal-variable}
                                        :return   {:ast/type :function
                                                   :domain   {:ast/type :universal-variable}
                                                   :return   {:ast/type :universal-variable}}}}}
          :principal]
         const))))

(deftest typecheck-typedefs
  (testing "Option"
    (let [{:strs [default map]}
          (->>
            (run :type-checker
              (defmodule lang.type-checker-test.type-definitions.option
                (:skip-implicits))
              (deftype (Option T)
                  (| [:none]
                     [:some T]))
              (defn default
                [option fallback]
                (match option
                  [:none] fallback
                  [:some value] value))
              (defn map
                [f option]
                (match option
                  [:none]       [:none]
                  [:some value] [:some (f value)])))
            :values
            (map (fn [[k v]] [(:name k) v]))
            (into {}))]

      (is (match?
           [{:ast/type :forall
             :variable {:ast/type :universal-variable}
             :body     {:ast/type :function
                        :domain
                        {:ast/type   :application
                         :operator   {:ast/type :named :name {:name "Option"}}
                         :parameters [{:ast/type :universal-variable}]}
                        :return
                        {:ast/type :function
                         :domain   {:ast/type :universal-variable}
                         :return   {:ast/type :universal-variable}}}}
            :principal]
           default))

      (is (match?
           [{:ast/type :forall
             :body
             {:ast/type :forall
              :body
              {:ast/type :function
               :domain   {:ast/type :function
                          :domain   {:ast/type :universal-variable}
                          :return   {:ast/type :universal-variable}}
               :return   {:ast/type :function
                          :domain   {:ast/type   :application
                                     :operator   {:ast/type :named :name {:name "Option"}}
                                     :parameters [{:ast/type :universal-variable}]}
                          :return   {:ast/type   :application
                                     :operator   {:ast/type :named :name {:name "Option"}}
                                     :parameters [{:ast/type :universal-variable}]}}}}}
            :principal]
           map))))

  (testing "List"
    (let [{:strs [map fold]}
          (->>
            (run :type-checker
              (defmodule lang.type-checker-test.type-definitions.list
                (:skip-implicits))
              (deftype (List T)
                  (| [:nil]
                     [:cons {:value T :next (List T)}]))
              (defn map
                [f list]
                (match list
                  [:nil] [:nil]
                  [:cons {:value hd :next tl}] [:cons {:value (f hd) :next (map f tl)}]))
              (defn fold
                [f init list]
                (match list
                  [:nil] init
                  [:cons {:value hd :next tl}] (f (fold f init tl) hd))))
            :values
            (map (fn [[k v]] [(:name k) v]))
            (into {}))]

      (is (match?
           [{:ast/type :forall
             :body
             {:ast/type :forall
              :body
              {:ast/type :function
               :domain   {:ast/type :function}
               :return   {:ast/type :function
                          :domain   {:ast/type :application}
                          :return   {:ast/type :application}}}}}
            :principal]
           map))

      (is (match?
           [{:ast/type :forall
             :body
             {:ast/type :forall
              :body
              {:ast/type :function
               :domain
               {:ast/type :function
                :domain   {:ast/type :universal-variable}
                :return
                {:ast/type :function
                 :domain   {:ast/type :universal-variable}
                 :return   {:ast/type :universal-variable}}}
               :return
               {:ast/type :function
                :domain   {:ast/type :universal-variable}
                :return
                {:ast/type :function
                 :domain
                 {:ast/type :application}
                 :return   {:ast/type :universal-variable}}}}}}
            :principal]
           fold)))))

(deftest typecheck-typeclasses
  (let [module
        (run :type-checker
          (defmodule lang.desugar.typeclasses-test.definitions
            (:skip-implicits))
          (defclass (Veracious T)
            (true? :$ (-> T Bool)))
          (definstance (Veracious Bool)
            (true? [bool] bool))
          (defn is-it-true? [x] (true? x))
          (def nope (is-it-true? false)))]

    (testing "types"
      (let [{:strs [is-it-true? nope]}
            (->> module
              :values
              (map (fn [[k v]] [(:name k) v]))
              (into {}))]

        (is (match?
             [{:ast/type :forall
               :body
               {:ast/type :guarded
                :proposition
                {:ast/constraint :instance
                 :typeclass      {:name "Veracious"}}
                :body
                {:ast/type :function
                 :domain   {:ast/type :universal-variable}
                 :return   {:ast/type :named :name {:name "Bool"}}}}}
              :principal]
             is-it-true?))

        (is (match?
             [{:ast/type :named :name {:name "Bool"}}
              :principal]
             nope))))

    (testing "AST"
      (let [[_ _ is-it-true? nope]
            (:definitions module)]
        (let [Bool {:ast/type :named
                    :name     {:name "Bool"}}]
          (is (match? {:ast/definition :constant
                       :name           {:name "is-it-true?"}
                       :body
                       {:ast/term               :recur
                        :body
                        {:ast/term  :lambda
                         :arguments [{:name "x"}]
                         :body
                         {:ast/term               :application
                          :function
                          {:type-checker.term/type
                           {:ast/type :forall
                            :body     {:ast/type :guarded
                                       :proposition
                                       {:ast/constraint :instance
                                        :typeclass
                                        {:reference :typeclass
                                         :name      "Veracious"
                                         :in
                                         {:reference :module
                                          :name      ["lang" "desugar" "typeclasses-test" "definitions"]}}
                                        :parameters
                                        [{:ast/type :universal-variable}]}
                                       :body     {:ast/type :function
                                                  :domain   {:ast/type :universal-variable}
                                                  :return   Bool}}}}
                          :arguments
                          [{:ast/term               :symbol
                            :type-checker.term/type {:ast/type :universal-variable}}]
                          :type-checker.term/type Bool}
                         :type-checker.term/type
                         {:ast/type :forall
                          :body
                          {:ast/type :guarded
                           :body     {:ast/type :function
                                      :domain   {:ast/type :universal-variable}
                                      :return   Bool}}}}
                        :type-checker.term/type {:ast/type :forall}}}
                      is-it-true?)))

        (is (match? {:ast/definition :constant
                     :name           {:name "nope"}
                     :body
                     {:ast/term  :application
                      :function  {:ast/term :symbol
                                  :symbol   {:name "is-it-true?"}
                                  :type-checker.term/type
                                  {:ast/type :guarded
                                   :proposition
                                   {:ast/constraint :instance
                                    :typeclass
                                    {:reference :typeclass
                                     :name      "Veracious"}
                                    :parameters
                                    [{:ast/type :named
                                      :name     {:name "Bool"}}]}
                                   :body     {:ast/type :function
                                              :domain   {:ast/type :named
                                                         :name     {:name "Bool"}}
                                              :return   {:ast/type :named
                                                         :name     {:name "Bool"}}}}}
                      :arguments [{:ast/term :atom
                                   :atom     {:atom :boolean :value false}}]}}
                    nope))))))
