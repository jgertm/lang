(ns lang.definition
  (:refer-clojure :exclude [name])
  (:require [clojure.string :as str]
            [lang.db :as db]))

(defn is?
  [form]
  (and (associative? form) (:ast/definition form)))

(defn type?
  [form]
  (-> form is? (= :type)))

(defn name
  [{definition-name :name :as form}]
  (case (:ast/definition form)
    :module
    (str/join "." (:name definition-name))

    :typeclass/instance
    (format "%s[%s]"
            (name (db/touch (:typeclass form)))
            (str/join ";" (mapv #(-> % :name db/touch name) (:types form))))

    (format "%s/%s"
            (name (db/touch (:in definition-name)))
            (:name definition-name))))
