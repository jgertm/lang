(ns lang.compiler
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lang.code-generator.jvm :as code-generator]
            [lang.desugar :as desugar]
            [lang.name-resolution :as name-resolution]
            [lang.parser :as parser]
            [lang.type-checker :as type-checker]
            [taoensso.timbre :as log]))

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
        (when-not (:skip-implicits module)
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
                   (-> module
                     (assoc
                       :alias alias
                       :open open)
                     (dissoc :definitions))))))))))

(defn run
  ([path]
   (run path :until :code-generator))
  ([path & {:keys [until]}]
   (try
     (log/debug "compiling module" path)
     (let [all-phases [:parser :dependency-analyzer :name-resolution :type-checker :desugar :code-generator]
           phases     (conj
                        (->> all-phases
                          (take-while (partial not= until))
                          (set))
                        until)
           result (cond-> path
                    (:parser phases)              (parser/run)
                    (:dependency-analyzer phases) (resolve-dependencies (last (vec (keep phases all-phases))))
                    (:name-resolution phases)     (name-resolution/run)
                    (:type-checker phases)        (type-checker/run)
                    (:desugar phases)             (desugar/run)
                    (:code-generator phases)      (code-generator/run :emit!))]
       (log/debug "done compiling" path)
       result)
     (catch Exception e
       (println (format "Error while compiling %s" path))
       (throw e)))))
