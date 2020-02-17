(ns lang.parser
  (:require [blancas.kern.core :as kern]
            [clojure.core.match :refer [match]]
            [lang.parser.definition :as definition]))

(def ^:private file
  (kern/bind [forms (kern/many definition/expr)]
    (match forms
      [({:ast/definition :module} :as module) & children]
      (kern/return (assoc module :definitions children)))))

(defn run
  [path]
  (let [result (kern/parse-file file path)]
    (if (:ok result)
      (:value result)
      (throw (ex-info "Parsing error" (:error result))))))
