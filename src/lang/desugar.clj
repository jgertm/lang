(ns lang.desugar
  (:require [lang.definition :as definition]
            [lang.desugar.macros :as macros]
            [lang.desugar.typeclasses :as typeclasses]
            [taoensso.timbre :as log]))

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
  (log/debug "desugaring" (definition/name module))
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
