(ns lang.utils
  (:require [com.gfredericks.debug-repl :refer [break!]]))

(defmacro undefined
  ;; (throw (Exception. "not implemented yet"))
  ([] `(break!))
  ([name] `(break! ~name)))

(defn concatv
  [& collections]
  (vec (apply concat collections)))

(def conjv
  (fnil conj []))
