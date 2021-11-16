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

(defn reference?
  [form]
  (and (map? form) (contains? form :ast/reference)))

(defn node?
  [form]
  ((some-fn definition? term? type? reference?) form))

(defn walk
  ([f ast] (walk f nil ast))
  ([f init ast]
   (walk f init nil ast))
  ([f init child ast]
   (if (node? ast)
     (let [[next ast] (f init child ast)]
       (with-meta
         (->> ast
              (map
               (fn [[k v]]
                 [k (cond
                      (node? v)
                      (walk f next [k] v)

                      (and (vector? v) (every? node? v))
                      (vec (map-indexed (fn [i v] (walk f next [k i] v)) v))

                      (map? v)
                      (->> v
                           (map
                            (fn [[l v]]
                              [(cond-> l (node? l) (walk f next [k l]))
                               (cond-> v (node? v) (walk f next [k l]))]))
                           (into {}))

                      :else v)]))
              (into {}))
         (meta ast)))
     ast)))

(defn map
  [f ast]
  (walk (fn [_ _ node] [nil (f node)]) nil ast))
