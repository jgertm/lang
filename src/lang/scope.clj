(ns lang.scope
  (:refer-clojure :exclude [assoc assoc-in empty get merge]))


(defn empty []
  '())

(defn enter
  [scope]
  (cons {} scope))

(defn exit
  [scope]
  (next scope))

(defn current
  [scope]
  (first scope))

(defn assoc-in
  [scope ks value]
  (-> scope
    (current)
    (clojure.core/assoc-in ks value)
    (cons (rest scope))))

(defn assoc
  [scope key value]
  (assoc-in scope [key] value))

(defn get
  [scope key]
  (some (fn [frame] (clojure.core/get frame key)) scope))

(defn merge [scope map]
  (-> scope
    (current)
    (clojure.core/merge map)
    (cons (rest scope))))
