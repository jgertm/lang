(ns lang.definition
  (:refer-clojure :exclude [name])
  (:require [clojure.string :as str]))

(defn is?
  [form]
  (and (associative? form) (:ast/definition form)))

(defn name
  [{:keys [name] :as form}]
  (letfn [(module-name [name] (str/join "." (:name name)))]
    (case (:ast/definition form)
      :module
      (module-name name)

      (format "%s/%s"
        (module-name (:in name))
        (:name name)))))
