(ns lang.desugar
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [lang.desugar.macros :as macros]
            [lang.desugar.typeclasses :as typeclasses]
            [lang.module :as module]
            [lang.term :as term]
            [lang.type :as type]
            [lang.utils :as utils :refer [undefined]]))

(defn- desugar
  [module definition]
  (->> definition
    (macros/desugar module)
    (typeclasses/desugar module)))

(defn- init
  [module]
  (merge module
    {:definitions                              []
     :desugar.typeclasses/dictionary-types     (atom {})
     :desugar.typeclasses/dictionary-arguments (atom {})
     :desugar.typeclasses/dictionary-instances (atom {})}))

(defn run
  [module]
  (reduce
    (fn [module definition]
      (update module :definitions conj (desugar module definition)))
    (init module)
    (:definitions module)))

(comment

  (do (println "\n–-—")
      (-> "examples/option.lang"
        (lang.compiler/run :until :desugar)
        :definitions
        #_(nth 2)))


  )
