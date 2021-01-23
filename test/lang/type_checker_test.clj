(ns lang.type-checker-test
  (:require [lang.test-prelude :refer :all]
            [lang.module :as module]))

(facts "atoms typecheck"
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

    (fact "foo" foo =>
      (matches [{:ast/type :named
                 :name     {:reference :type
                            :name      "Integer"
                            :in        {:reference :module :name ["lang" "builtin"]}}}
                :principal]))

    (fact "bar" bar =>
      (matches [{:ast/type :named :name {:name "String"}} :principal]))

    (fact "baz" baz =>
      (matches [{:ast/type :named :name {:name "Bool"}} :principal]))

    (fact "quux" quux =>
      (matches [{:ast/type :named :name {:name "Unit"}} :principal]))))


(facts "functions typecheck"
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

(facts "typedefs typecheck"
  (facts "Option"
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

      (fact "default" default =>
        (matches [{:ast/type :forall
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
                  :principal]))

      (fact "map" map =>
        (matches [{:ast/type :forall
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
                  :principal]))))

  (facts "List"
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

      (fact "map" map =>
        (matches [{:ast/type :forall
                   :body
                   {:ast/type :forall
                    :body
                    {:ast/type :function
                     :domain   {:ast/type :function}
                     :return   {:ast/type :function
                                :domain   {:ast/type :application}
                                :return   {:ast/type :application}}}}}
                  :principal]))

      (fact "fold" fold =>
        (matches [{:ast/type :forall
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
                  :principal])))))

(facts "typeclasses typecheck"
  (let [module
        (run :type-checker
          (defmodule lang.desugar.typeclasses-test.definitions
            (:skip-implicits))
          (defclass (Veracious T)
            (true? :$ (-> T Bool)))
          (definstance (Veracious Bool)
            (true? [bool] bool))
          (defn is-it-true? [x] (true? x)) ; works
          (def nope (is-it-true? false))   ; works
          )]

    (facts "types"
      (let [{:strs [is-it-true? nope]}
            (->> module
              :values
              (map (fn [[k v]] [(:name k) v]))
              (into {}))]

        (fact "is-it-true?" is-it-true? =>
          (matches [{:ast/type :forall
                     :body
                     {:ast/type :guarded
                      :proposition
                      {:ast/constraint :instance
                       :typeclass      {:name "Veracious"}}
                      :body
                      {:ast/type :function
                       :domain   {:ast/type :universal-variable}
                       :return   {:ast/type :named :name {:name "Bool"}}}}}
                    :principal]))))

    (facts "AST"
      (let [[_ _ is-it-true? nope]
            (:definitions module)]
        (fact "is-it-true?" is-it-true? =>
          (let [Bool {:ast/type :named
                      :name     {:name "Bool"}}]
            (matches {:ast/definition :constant
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
                       :type-checker.term/type {:ast/type :forall}}})))

        (fact "nope" nope =>
          (matches {:ast/definition :constant
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
                                  :atom     {:atom :boolean :value false}}]}}))))))
