(ns lang.term
  (:refer-clojure :exclude [symbol? type]))

(defn is?
  [form]
  (and (map? form) (:ast/term form)))

(defn lambda?
  [form]
  (-> form is? (= :lambda)))

(defn application?
  [form]
  (-> form is? (= :application)))

(defn symbol?
  [form]
  (-> form is? (= :symbol)))

(defn type
  [form]
  {:pre [(is? form)]}
  (:type-checker.term/type form))
