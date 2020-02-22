(ns lang.compiler
  (:require [lang.code-generator :as code-generator]
            [lang.parser :as parser]
            [lang.type-checker :as type-checker]))

(defn run
  [path]
  (-> path
    (parser/run)
    (type-checker/run)
    (code-generator/run)))
