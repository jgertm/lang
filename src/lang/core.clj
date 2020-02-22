(ns lang.core
  (:gen-class)
  (:require [clojure.string :as str]
            [lang.compiler :as compiler]))

(comment

  (compiler/run "../lang/examples/option.lang")

  )

(defn -main
  [argv]
  (-> argv
    (str/split #"\s+")
    (first)
    (compiler/run)))
