(ns lang.state
  (:require [clojure.core.cache.wrapped :as cache]
            [taoensso.timbre :as log]))

(def state (atom {}))

(defn void! []
  (reset! state {}))

(defn get! [path]
  (reduce
   (fn [val step]
     (if (delay? val)
       (get @val step)
       (get val step)))
   @state
   path))

(def cache (cache/lru-cache-factory {}))

(def ^:dynamic *queries* nil)
(def ^:dynamic *inputs* nil)

(defmacro definput [name params & body]
  `(defn ~name
     ~params
     ~@body))

(defmacro defquery [name params & body]
  `(defn ~name
     ~params
     (let [subqueries# (atom {})
           key#
           ~{:query (intern *ns* name)
             :args  (mapv #(vector (list `quote %) %) params)}]
       ;; TODO: check subquery results
       (let [record#
             (cache/lookup-or-miss cache key#
                                   (fn [key#]
                                     {:result     (binding [*queries* subqueries#] ~@body)
                                      :subqueries (->> @subqueries#
                                                       (map (fn [[k# v#]] [k# (:result v#)]))
                                                       (into {}))}))]
         (some-> *queries* (swap! assoc key# record#))
         (:result record#)))))

@cache
