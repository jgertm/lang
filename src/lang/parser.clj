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
  (->> path
    (kern/parse-file file)
    :value))
