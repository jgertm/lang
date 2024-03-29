(ns lang.desugar.typeclasses-test
  (:require [clojure.test :refer [deftest is testing]]
            [lang.test-prelude :refer :all]
            matcher-combinators.test))

(deftest desugar-simple-typeclass
  (let [[class instance is-it-true? nope]
        (->
         (run :desugar
           (defmodule lang.desugar.typeclasses-test.simple-definitions
             (:skip-implicits))
           (defclass (Veracious T)
             (true? :$ (-> T Bool)))
           (definstance (Veracious Bool)
             (true? [bool] bool))
           (defn is-it-true? [x] (true? x))
           (def nope (is-it-true? false)))
         :definitions)
        module {:reference :module
                :name      ["lang" "desugar" "typeclasses-test" "simple-definitions"]}
        Bool   {:ast/type :named
                :name
                {:reference :type :name "Bool"}}]

    (is (match? {:ast/definition :type
                 :name           {:reference :type :name "D:Veracious"}
                 :params         [{:name "T"}]
                 :body
                 {:ast/type :forall
                  :variable {:ast/type  :universal-variable
                             :reference {:name "T"}}
                  :body     {:ast/type :record
                             :fields
                             {{:reference :field
                               :name      "true?"
                               :in        module}
                              {:ast/type :function
                               :domain   {:ast/type  :universal-variable
                                          :reference {:reference :type :name "T"}}
                               :return
                               Bool}}}}}
                class))

    (is (match? {:ast/definition :constant
                 :name           {:reference :constant :name "I:Veracious:Bool"}
                 :body
                 {:ast/term :record
                  :fields
                  {{:reference :field
                    :name      "true?"
                    :in        module}
                   {:ast/term  :lambda
                    :arguments [{:reference :constant :name "bool"}]
                    :body
                    {:ast/term :symbol
                     :symbol   {:reference :constant :name "bool"}
                     :type-checker.term/type
                     {:ast/type :named :name {:name "Bool"}}}
                    :type-checker.term/type
                    {:ast/type :function
                     :domain   Bool
                     :return   Bool}}}}}
                instance))

    (let [dictionary {:reference :dictionary
                      :name      #(->> % (name) (re-find #"Veracious"))}
          argument   {:reference :constant
                      :name      "x"}]
      (is (match? {:ast/definition :constant
                   :name           {:name "is-it-true?"}
                   :body           {:ast/term :recur
                                    :body
                                    {:ast/term  :lambda
                                     :arguments [dictionary argument]
                                     :body
                                     {:ast/term  :application
                                      :function  {:ast/term :extract
                                                  :record   {:ast/term :symbol :symbol dictionary}
                                                  :field    {:reference :field :name "true?"}}
                                      :arguments [{:ast/term :symbol :symbol argument}]}}}}
                  is-it-true?)))

    (is (match? {:ast/definition :constant
                 :name           {:name "nope"}
                 :body           {:ast/term  :application
                                  :function  {:ast/term :symbol
                                              :symbol   {:name "is-it-true?"}}
                                  :arguments [{:ast/term :symbol
                                               :symbol   {:reference :constant
                                                          :in        module
                                                          :name      "I:Veracious:Bool"}}
                                              {:ast/term :atom
                                               :atom     {:atom :boolean :value false}}]}}
                nope))))

(deftest desugar-chained-typeclasses
  (let [module
        (run :desugar
          (defmodule lang.desugar.typeclasses-test.chained-definitions
            (:skip-implicits))
          (defclass (Show T)
            (show :$ (-> T String)))
          (deftype (Identity T)
              (| [:value T]))
          (definstance (Show Integer)
            (show [i]
                  (. i (java.math.BigInteger/toString))))
          (definstance (Show (Identity T))
            :when (Show T)
            (show [identity]
                  (match identity
                         [:value val] (show val))))
          (def just-a-number
            (show [:value 42]))
          (defn wrap-and-show
            [value]
            (show [:value value])))
        [class type integer-instance identity-instance just-a-number wrap-and-show]
        (:definitions module)]

    (is
     (match? {:ast/definition :constant
              :body
              {:ast/term :lambda
               :arguments [{:reference :dictionary}]
               :body
               {:ast/term :record
                :fields   {{:reference :field :name "show"
                            :in        {:reference :module :name ["lang" "desugar" "typeclasses-test" "chained-definitions"]}}
                           {:ast/term :lambda
                            :body
                            {:ast/term :match
                             :branches
                             [{:action {:ast/term  :application
                                        :function  {:ast/term :extract
                                                    :record   {:ast/term :symbol
                                                               :symbol   {:reference :dictionary}}}
                                        :arguments [{:ast/term :symbol}]}}]}}}}}}
             identity-instance))

    (is
     (match? {:ast/definition :constant
              :body
              {:ast/term :application
               :function {:ast/term :extract
                          :record   {:ast/term  :application
                                     :function  {:ast/term :symbol
                                                 :symbol   {:name "I:Show:(Identity α)"}}
                                     :arguments [{:ast/term :symbol
                                                  :symbol   {:name "I:Show:Integer"}}]}}}}
             just-a-number))

    (is
     (match? {:ast/definition :constant
              :body
              {:ast/term :recur
               :body
               {:ast/term :lambda
                :arguments [{:reference :dictionary} {:reference :constant}]
                :body
                {:ast/term  :application
                 :function  {:ast/term :extract
                             :record   {:ast/term  :application
                                        :function  {:ast/term :symbol
                                                    :symbol   {:name "I:Show:(Identity α)"}}
                                        :arguments [{:ast/term :symbol
                                                     :symbol   {:reference :dictionary}}]}}
                 :arguments [{:ast/term :variant}]}}}}
             wrap-and-show))))
