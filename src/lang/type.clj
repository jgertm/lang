(ns lang.type
  (:require [clojure.core.match :refer [match]]))

(defn injectors
  [{:keys [name body]}]
  (letfn [(extract [type]
            (match type
             {:ast/type :variant :injectors injectors}
             (->> injectors
               (map (fn [[k v]] [k #:type{:inner v :outer body :name name}]))
               (into {}))

             {:ast/type :forall}
             (recur (:body type))

             _ nil))]
    (extract body)))


