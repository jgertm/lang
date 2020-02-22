(ns lang.utils
  (:require [com.gfredericks.debug-repl :refer [break! unbreak! unbreak!!]]))

(defmacro undefined
  ;; (throw (Exception. "not implemented yet"))
  ([] `(break!))
  ([name] `(break! ~name)))
