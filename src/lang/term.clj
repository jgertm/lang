(ns lang.term
  (:refer-clojure :exclude [walk]))

(defn is?
  [form]
  (:ast/term form))
