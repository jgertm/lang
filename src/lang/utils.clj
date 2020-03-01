(ns lang.utils
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

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
