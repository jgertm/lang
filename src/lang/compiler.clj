(ns lang.compiler
  (:require [lang.parser :as parser]
            [lang.type-checker :as type-checker]))

(defn run
  [path]
  (-> path
    (parser/run)
    (type-checker/run)))
