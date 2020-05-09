(ns lang.compiler
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lang.code-generator :as code-generator]
            [lang.name-resolution :as name-resolution]
            [lang.parser :as parser]
            [lang.type-checker :as type-checker]))

(declare run)

(def lang-home 
  (or
    (System/getenv "LANG_HOME")
    "/home/tjgr/Dropbox/lang-clj/std"
    (throw (ex-info "Could not find stdlib" {}))))

(defn- resolve-dependencies
  [module phases]
  (update module :imports
    (fn [imports]
      (map (fn [{:keys [module alias]}]
            (let [file (when (= "lang" (first (:name module)))
                         (-> "/"
                           (str/join (cons lang-home (:name module)))
                           (str ".lang")
                           (io/file)))]
              (assoc (run file phases) :alias alias))) imports))))

(defn run
  ([path]
   (run path #{:parser :name-resolution :dependency-analyzer :type-checker :code-generator}))
  ([path phases]
   (cond-> path
     (:parser phases) (parser/run)
     (:name-resolution phases) (name-resolution/run)
     (:dependency-analyzer phases) (resolve-dependencies (conj phases :type-checker))
     (:type-checker phases) (type-checker/run)
     (:code-generator phases) (code-generator/run))))
