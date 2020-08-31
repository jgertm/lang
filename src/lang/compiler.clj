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
    (throw (ex-info "Could not find stdlib" {}))))

(def ^:private default-imports
  (->> [["lang" "core"]
        ["lang" "math"]]
    (map (fn [name]
           {:reference :module
            :name      name}))
    (set)))

(defn- resolve-dependencies
  [module phase]
  (let [additional-imports
        (when-not (contains? default-imports (:name module))
          (map
            (fn [m] {:module m :open true})
            default-imports))]
    (update module :imports
      (fn [imports]
        (->> imports
          (concat additional-imports)
          (map (fn [{:keys [module alias open]}]
                 (let [file (when (= "lang" (first (:name module)))
                              (-> "/"
                                (str/join (cons lang-home (:name module)))
                                (str ".lang")
                                (io/file)))
                       module (run file :until phase)]
                   (assoc module
                     :alias alias
                     :open open)))))))))

(defn run
  ([path]
   (run path :until :code-generator))
  ([path & {:keys [until]}]
   (let [all-phases [:parser :name-resolution :dependency-analyzer :type-checker :code-generator]
         phases     (conj
                      (->> all-phases
                        (take-while (partial not= until))
                        (set))
                      until)]
     (cond-> path
       (:parser phases)              (parser/run)
       (:name-resolution phases)     (name-resolution/run)
       (:dependency-analyzer phases) (resolve-dependencies
                                       (or (:code-generator phases) :type-checker))
       (:type-checker phases)        (type-checker/run)
       (:code-generator phases)      (code-generator/run)))))
