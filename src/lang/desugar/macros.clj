(ns lang.desugar.macros
  (:require [clojure.walk :as walk]))

(defn desugar
  [module definition]
  (walk/prewalk
    (fn [node]
      (if-let [expansion (:type-checker.macro/expands-to node)]
        (recur expansion)
        node))
    definition))
