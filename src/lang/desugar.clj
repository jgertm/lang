(ns lang.desugar
  (:require [lang.definition :as definition]
            [lang.desugar.typeclasses :as typeclasses]
            [taoensso.timbre :as log]
            [lang.db :as db]))

(defn- desugar
  [module definition]
  (->> definition
    (typeclasses/desugar module)))

(defn- init
  [module]
  (merge module
    {:desugar.typeclasses/dictionary-types     (atom {})
     :desugar.typeclasses/dictionary-arguments (atom {})
     :desugar.typeclasses/dictionary-instances (atom {})}))

(defn run
  [module]
  (log/debug "desugaring" (definition/name module))
  (let [module (init module)]
    (run!
     (fn [definition]
       (db/tx! db/state
               [(desugar module definition)]
               {:lang.compiler/pass [::run (:ast/definition definition)]}))
    (:definitions module)))
  {:db-after @db/state})

(comment


  (let [module {:ast/reference :module :name ["lang" "core"]}]
    (#'lang.compiler/init)
    (-> (#'lang.compiler/run module)
        #_#_(dissoc :macros :typeclasses)
        (update :definitions (partial filterv #(-> % :ast/definition #{:typeclass/declaration :typeclass/instance})))))

  (com.gfredericks.debug-repl/unbreak!!)

  )
