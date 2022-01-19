(ns lang.parser
  (:require [blancas.kern.core :as kern]
            [clojure.core.match :refer [match]]
            [lang.db :as db]
            [lang.parser.definition :as definition]
            [taoensso.timbre :as log]))

(def ^:private file
  (kern/bind [forms (kern/many definition/expr)]
    (match forms
      [({:ast/definition :module} :as module) & children]
      (kern/return (update module :definitions (fnil into []) children)))))

(defn run
  [text path]
  (log/debug "parsing file" path)
  (let [result (kern/parse file text path)]
    (if (:ok result)
      (:value result)
      (throw (ex-info "Parsing error" (:error result))))))

(comment
  (db/tx! db/state
                [(#'lang.compiler/prepare-ast
                  (run (slurp (clojure.java.io/resource "lang/io.lang")) "std/lang/io.lang"))

                 ])

  (db/datoms @db/state)

  (:reference (:body (db/->entity @db/state 10)))

  )
