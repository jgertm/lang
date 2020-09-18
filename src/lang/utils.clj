(ns lang.utils
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def dev?
  (= :dev (:profile (edn/read-string (slurp (io/resource "config.edn"))))))

(when dev?
  (require '[com.gfredericks.debug-repl :refer [break!]]))

(defmacro undefined
  ;; (throw (Exception. "not implemented yet"))
  ([]
   (if dev?
     `(break!)
     `(throw (Exception. "not implemented yet"))))
  ([name]
   (if dev?
     `(break! ~name)
     `(throw (Exception. "not implemented yet")))))

(defn concatv
  [& collections]
  (vec (apply concat collections)))

(def conjv
  (fnil conj []))

(defn deep-merge
  [& maps]
  (apply
    (fn m [& maps]
      (cond
        (every? map? maps)
        (apply merge-with m maps)

        (every? set? maps)
        (apply set/union maps)

        (every? vector? maps)
        (reduce into maps)

        :else (last maps)))
    maps))

(defn spy
  [f v]
  (f v)
  v)

(defn symmetric-difference
  [a b]
  (set/difference (set/union a b) (set/intersection a b)))
