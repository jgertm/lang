(ns lang.desugar.typeclasses-test
  (:require [lang.test-prelude :refer :all]))

(facts "simple typeclasses desugar"
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

    (fact "class" class =>
      (matches {:ast/definition :type
                :name           {:reference :type :name "D:Veracious"}
                :params         [{:name "T"}]
                :body 
                {:ast/type :record
                 :fields
                 {{:reference :field
                   :name      "true?"
                   :in        module}
                  {:ast/type :function
                   :domain   {:ast/type :named :name {:reference :type :name "T"}}
                   :return
                   Bool}}}}))

    (fact "instance" instance =>
      (matches {:ast/definition :constant
                :name           {:reference :constant :name "I:Veracious:Bool"}
                :body
                {:ast/term :record
                 :fields
                 {{:reference :field
                   :name      "true?"
                   :in        module}
                  {:ast/term :lambda
                   :argument {:reference :constant :name "bool"}
                   :body
                   {:ast/term               :sequence
                    :operations             [{:ast/term :symbol
                                              :symbol   {:reference :constant :name "bool"}
                                              :type-checker.term/type
                                              {:ast/type :primitive :primitive :boolean}}]
                    :type-checker.term/type Bool}
                   :type-checker.term/type
                   {:ast/type :function
                    :domain   Bool
                    :return   Bool}}}}}))

    (fact "is-it-true?" is-it-true? =>
      (let [dictionary {:reference :dictionary
                        :name      #(->> % (name) (re-find #"Veracious"))}
            argument   {:reference :constant
                        :name      "x"}]
        (matches {:ast/definition :constant
                  :name           {:name "is-it-true?"}
                  :body           {:ast/term :recur
                                   :body
                                   {:ast/term :lambda
                                    :argument dictionary
                                    :body
                                    {:ast/term :lambda
                                     :argument argument
                                     :body
                                     {:ast/term   :sequence
                                      :operations [{:ast/term  :application
                                                    :function  {:ast/term :extract
                                                                :record   {:ast/term :symbol :symbol dictionary}
                                                                :field    {:reference :field :name "true?"}}
                                                    :arguments [{:ast/term :symbol :symbol argument}]}]}}}}})))

    (fact "nope" nope =>
      (matches {:ast/definition :constant
                :name           {:name "nope"}
                :body           {:ast/term  :application
                                 :function  {:ast/term :symbol
                                             :symbol   {:name "is-it-true?"}}
                                 :arguments [{:ast/term :symbol
                                              :symbol   {:reference :constant
                                                         :in        module
                                                         :name      "I:Veracious:Bool"}}
                                             {:ast/term :atom
                                              :atom     {:atom :boolean :value false}}]}}))))

