(ns lang.desugar
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn- desugar-macros
  [definition]
  (walk/prewalk
    (fn [node]
      (if-let [expansion (:type-checker.macro/expands-to node)]
        (recur expansion)
        node))
    definition))

(defn- typeclass-record-name
  [name]
  (-> name
    (assoc :reference :type)
    (update :name #(format "D:%s" %))))

(defn- desugar-typeclasses
  [definition]
  (match definition
    {:ast/definition :typeclass}
    {:ast/definition :type
     :name           (typeclass-record-name (:name definition))
     :params         (:params definition)
     :body           {:ast/type :record
                      :fields   (->> definition
                                  :fields
                                  (mapv (fn [[name type]]
                                          [(assoc name :reference :field) type]))
                                  (into (empty (:fields definition))))}}

    {:ast/definition :typeclass-instance}
    (let [types (->> definition
                  :types
                  (map (comp :name :name))
                  (str/join "-"))]
      {:ast/definition :constant
       :name           (-> definition
                         :name
                         (assoc :reference :variable)
                         (update :name #(format "I:%s:%s" % types)))
       :body           {:ast/term :record
                        :fields   (->> definition
                                  :fields
                                  (map (fn [[name term]]
                                         [(assoc name :reference :field) term]))
                                  (into (empty (:fields definition))))}
       :type-checker.term/type
       {:ast/type   :application
        :operator   {:ast/type :named :name (typeclass-record-name (:name definition))}
        :parameters (:types definition)}})

    {:ast/definition :constant}
        nil

        _ definition))

(defn- desugar
  [definition]
  (-> definition
    (desugar-macros)
    (desugar-typeclasses)))

(defn run
  [module]
  (reduce
    (fn [module definition]
      (update module :definitions conj (desugar definition)))
    (assoc module :definitions [])
    (:definitions module)))

(comment

  (do (println "\n–-—")
      (-> "std/lang/option.lang"
        (lang.compiler/run :until :type-checker)
        :definitions
        #_module/surface-bindings))

  )
