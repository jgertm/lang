(ns lang.ast
  (:require [clojure.core.match :refer [match]]
            [lang.term :as term]
            [lang.type :as type]
            [lang.db :as db]
            [lang.pattern :as pattern]
            [lang.definition :as definition]
            [lang.utils :refer [undefined]]))

(defn definition?
  [form]
  (definition/is? form))

(defn module?
  [form]
  (and
   (definition? form)
   (= :module (get form :ast/definition))))

(defn term?
  [form]
  (term/is? form))

(defn type?
  [form]
  (type/is? form))

(defn reference?
  [form]
  (and (associative? form)
       (contains? form :ast/reference)))

(defn pattern?
  [form]
  (pattern/is? form))

(defn node?
  [form]
  ((some-fn definition? term? type? pattern? reference?) form))

(defn walk
  ([f ast] (walk f nil ast))
  ([f init ast]
   (cond
     (node? ast)
     (let [[next ast] (f init ast)]
       (if (node? ast)
         (->> ast
              (map (fn [[k v]] [k (walk f next v)]))
              (into (empty ast)))
         ast))

     (and (map? ast) (not (db/ref? ast)))
     (->> ast
          (map (fn [[k v]]
                 [(walk f init k) (walk f init v)]))
          (into (empty ast)))

     (and (coll? ast) (not (db/ref? ast)))
     (->> ast
          (map (fn [v] (walk f init v)))
          (into (empty ast)))

     :else ast)))
