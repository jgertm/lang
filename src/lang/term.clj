(ns lang.term
  (:refer-clojure :exclude [symbol? type record?])
  (:require [clojure.core.match :refer [match]]))

(defn is?
  [form]
  (and (associative? form) (:ast/term form)))

(defn lambda?
  [form]
  (-> form is? (= :lambda)))

(defn application?
  [form]
  (-> form is? (= :application)))

(defn symbol?
  [form]
  (-> form is? (= :symbol)))

(defn atom?
  [form]
  (-> form is? (= :atom)))

(defn match?
  [form]
  (-> form is? (= :match)))

(defn record?
  [form]
  (-> form is? (= :record)))

(defn type
  [form]
  ;; {:pre [(is? form)]}
  (:type-checker.term/type form))

(defn children
  [node]
  (match node
    {:ast/term :record :fields fields}
    (vals fields)

    {:ast/term :application :function function :arguments arguments}
    (cons function arguments)

    {:ast/term :match :body body :branches branches}
    (cons body (map :action branches))

    {:ast/term :sequence :operations operations}
    operations

    _
    (->> node
      (vals)
      (filter is?))))

(defn nodes
  [term]
  (tree-seq is? children term))
