(ns lang.parser
  (:require [blancas.kern.core :as kern]
            [clojure.core.match :refer [match]]
            [lang.parser.definition :as definition]
            [taoensso.timbre :as log]))

(defn run-expr [s]
  (let [{:keys [error ok value]} (kern/parse definition/expr s)]
    (if ok 
      value
      (throw (ex-info "Failed to parse expr" error)))))

(def ^:private file
  (kern/bind [forms (kern/many definition/expr)]
    (match forms
      [({:ast/definition :module} :as module) & children]
      (kern/return (update module :definitions (fnil into []) children)))))

(defn run
  [path]
  (log/debug "parsing file" path)
  (let [result (kern/parse-file file path)]
    (if (:ok result)
      (:value result)
      (throw (ex-info "Parsing error" (:error result))))))

(comment
  (run-expr "(def a 1)"))
