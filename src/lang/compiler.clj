(ns lang.compiler
  (:require [lang.code-generator :as code-generator]
            [lang.parser :as parser]
            [lang.type-checker :as type-checker]))

(defn run
  ([path]
   (run path #{:parser :type-checker :code-generator}))
  ([path phases]
   (cond-> path
     (:parser phases) (parser/run)
     (:type-checker phases) (type-checker/run)
     (:code-generator phases) (code-generator/run))))
