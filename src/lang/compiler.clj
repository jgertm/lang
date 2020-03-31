(ns lang.compiler
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lang.code-generator :as code-generator]
            [lang.parser :as parser]
            [lang.type-checker :as type-checker]))

(declare run)

(def ^:private lang-home "/home/tjgr/Dropbox/lang-clj/std") ; TODO: env var

(defn- resolve-dependencies
  [module]
  (update module :imports
    (fn [imports]
      (map (fn [{:keys [module alias]}]
            (let [file (when (= "lang" (first (:name module)))
                         (-> "/"
                           (str/join (cons lang-home (:name module)))
                           (str ".lang")
                           (io/file)))]
              (assoc (run file) :alias alias))) imports))))

(defn run
  ([path]
   (run path #{:parser :dependency-analyzer :type-checker :code-generator}))
  ([path phases]
   (cond-> path
     (:parser phases) (parser/run)
     (:dependency-analyzer phases) (resolve-dependencies)
     (:type-checker phases) (type-checker/run)
     (:code-generator phases) (code-generator/run))))
