(ns lang.parser
  (:require [blancas.kern.core :as kern]
            [clojure.core.match :refer [match]]
            [lang.parser.definition :as definition]
            [lang.state :refer [definput defquery]]
            [taoensso.timbre :as log]))

(def ^:private file
  (kern/bind [forms (kern/many definition/expr)]
    (match forms
      [({:ast/definition :module} :as module) & children]
      (kern/return (update module :definitions (fnil into []) children)))))

(defn run
  [path text]
  (log/debug "parsing file" path)
  (let [result (kern/parse file text path)]
    (if (:ok result)
      (:value result)
      (throw (ex-info "Parsing error" (:error result))))))

(defquery source [key]
  (log/debug "reading file" key)
  (match key
         [:file path]
         (slurp path)))

(defquery ast [key]
   (log/debug "parsing ast" key)
   (let [text (source key)
         result (kern/parse file text)]
     (if (:ok result)
       (:value result)
       (throw (ex-info "Parsing error" (:error result))))))

(defquery definitions [key]
  (log/debug "extracting definitions" key)
  (->> key
       ast
       :definitions
       (map (juxt :ast/definition (comp :name :name)))))

(comment

  (definitions [:file "std/lang/string.lang"])

  (ast [:file "std/lang/string.lang"])

  (definitions [:file "std/lang/option.lang"])

  )
