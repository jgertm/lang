(ns lang.ast
  (:require [lang.term :as term]
            [lang.type :as type]))

(defn module?
  [form]
  (and
    (map? form)
    (= :module (get form :ast/definition))))

(defn definition?
  [form]
  (and
    (map? form)
    (contains? form :ast/definition)))

(defn term?
  [form]
  (term/is? form))

(defn type?
  [form]
  (type/is? form))

(defn node?
  [form]
  ((some-fn definition? term? type?) form))
