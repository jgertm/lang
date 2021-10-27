(ns lang.pattern
  (:refer-clojure :exclude [record?])
  (:require [clojure.core.match :refer [match]]))

(defn is?
  [form]
  (and (map? form) (:ast/pattern form)))

(defn record?
  [form]
  (-> form is? (= :record)))

(defn children
  [node]
  (match node
    {:ast/pattern :variant :value value}
    (filter some? [value])

    {:ast/pattern :record :fields fields}
    (vals fields)

    _
    (->> node
      (vals)
      (filter is?))))

(defn nodes
  [pattern]
  (tree-seq is? children pattern))
