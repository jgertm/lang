(ns lang.compiler
  (:require [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [lang.ast :as ast]
            [lang.parser :as parser]
            [lang.state :as state :refer [state]]
            [taoensso.timbre :as log]))

(declare run)

(def ^:private default-imports
  (->> [["lang" "core"]
        ["lang" "math"]]
    (map (fn [name]
           {:ast/reference :module
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
                                (str/join (:name module))
                                (str ".lang")
                                (io/resource)))
                       module (run file :until phase)]
                   (-> module
                     (assoc
                       :alias alias
                       :open open)
                     (dissoc :definitions))))))))))

(defn- inject-ids
  [key ast]
  (ast/walk
   (fn [id child node]
     (let [id
           (if (ast/node? node)
             (into id child)
             id)]
       [id
        (vary-meta node assoc :ast/id id)]))
   [key :ast]
   ast))

(defn- inline-meta
  ([ast] (inline-meta ast nil))
  ([ast keys]
   (ast/walk
    (fn [_ _ node]
      [nil (merge node (cond-> (meta node) (not-empty keys) (select-keys keys)))])
    ast)))

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
           #_#_result (cond-> path
                        (:parser phases)              (parser/run)
                        (:dependency-analyzer phases) (resolve-dependencies (last (vec (keep phases all-phases))))
                        (:name-resolution phases)     (name-resolution/run)
                        (:type-checker phases)        (type-checker/run)
                        (:desugar phases)             (desugar/run)
                        (:code-generator phases)      (code-generator/run :emit!))
           key [:file path]]
       (swap! state update key
              (fn [file-state]
                (assoc file-state
                       :text (delay (slurp path))
                       :ast (delay (->> @(get-in @state [key :text])
                                        (parser/run key)
                                        (inject-ids key)))
                       :imports (delay (mapv
                                        (fn [{:keys [module]}]
                                          (run (format "%s.lang" (str/join "/" (cons "std" (:name module))))))
                                        (:imports @(get-in @state [key :ast])))))))
       key)
     (catch Exception e
       (println (format "Error while compiling %s" path))
       (throw e)))))


(comment

  (do (state/void!)
      (run "std/lang/string.lang")
      @state
      (->> @(get-in @state [[:file "std/lang/string.lang"] :ast])
           inline-meta))


  (meta (map identity (with-meta [1 2 3] {:foo "bar"})))

  @state

  (walk/prewalk
   (fn [node] (if (delay? node) @node node))
   @state)


  )
