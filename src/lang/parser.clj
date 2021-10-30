(ns lang.parser
  (:require [blancas.kern.core :as kern]
            [clojure.core.match :refer [match]]
            [lang.parser.definition :as definition]
            [taoensso.timbre :as log]))

(def ^:private file
  (kern/bind [forms (kern/many definition/expr)]
    (match forms
      [({:ast/definition :module} :as module) & children]
      (kern/return (update module :definitions (fnil into []) children)))))

(defn run
  [path text]
  (log/debug "parsing file" path)
  (let [result (kern/parse file text path)]
    (if (:ok result)
      (:value result)
      (throw (ex-info "Parsing error" (:error result))))))
