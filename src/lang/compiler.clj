(ns lang.compiler
  (:require [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [lang.ast :as ast]
            [lang.db :as db]
            [lang.parser :as parser]
            [lang.name-resolution :as name-resolution]
            [lang.type-checker :as type-checker]
            [lang.utils :refer [undefined]]
            [lang.desugar :as desugar]
            [lang.code-generator.jvm :as code-generator]
            [taoensso.timbre :as log]))

(declare run)

(def ^:private native-module
  (let [atoms
        {"Unit"    [:unit {:jvm/class Void/TYPE}]
         "String"  [:string {:jvm/class String}]
         "Integer" [:integer {:jvm/class BigInteger}]
         "Bool"    [:boolean {:jvm/class Boolean}]
         "Object"  [:object {:jvm/class Object}]}
        primitives
        {"int"  [:int {:jvm/class Integer/TYPE}]
         "bool" [:bool {:jvm/class Boolean/TYPE}]}]
    {:ast/definition :module
     :name           {:ast/reference :module :name ["lang" "native" "jvm"]}
     :skip-implicits true
     :definitions
     (->> primitives
          (merge atoms)
          (map (fn [[k [v opts]]]
                 [k
                  (merge
                   {:ast/type  :primitive
                    :primitive v}
                   opts)]))
          ;; TODO(tjaeger): maybe add a param here
          ;; TODO: add/derive class references
          (cons ["Array"
                 (let [uvar (gensym)]
                   {:ast/type :forall
                    :variable uvar
                    :body     {:ast/type  :primitive
                               :primitive :array
                               :element   uvar}})])
          (map (fn [[k v]] {:ast/definition :type
                            :name           {:ast/reference :type :name k}
                            :body           v}))
          vec)}))

(defn init []
  (reset! db/state (db/init))
  (let [{{eid -1} :tempids db :db-after}
        (db/tx! db/state
                [(-> native-module
                     (update :definitions (partial mapv #(assoc % :db/id (gensym))))
                     (assoc :db/id -1))]
                {::pass :boot})]
    (name-resolution/run (db/touch (db/->entity db eid)))))

(def ^:private default-imports
  #{{:ast/reference :module :name ["lang" "core"]}
    {:ast/reference :module :name ["lang" "math"]}})

(defn prepare-ast
  [ast root-eid]
  (merge
   (walk/postwalk
    (fn [node]
      (cond
        (or (ast/definition? node)
            (ast/term? node)
            ;; (ast/type? node)
            (ast/pattern? node))
        (assoc node :db/id (gensym))

        :else node))
    ast)
   {:db/id root-eid}))

;; TODO: only expect eid in passes instead of whole-ass AST
;; TODO: support only running some phases
(defn run
  [{:keys [name] :as module}]
  (or (some-> @db/state (db/->entity [:name module]) db/touch)
      (let [path
            (if (= "lang" (first name))
              (io/resource (format "%s.lang" (str/join "/" name)))
              (apply io/file (System/getProperty "user.dir") name))
            {{eid -1} :tempids}
            (db/tx! db/state [{:db/id -1 :path path}] {::pass :init})
            {db :db-after :as result}
            (db/tx! db/state
                    [(-> path slurp (parser/run (.getPath path)) (prepare-ast eid))]
                    {::pass :parser})
            parser-ast (db/touch (db/->entity db eid))
            imports (concat [{:module {:ast/reference :module
                                       :name ["lang" "native" "jvm"]}
                              :open true}]
                            (:imports parser-ast)
                            (when-not (:skip-implicits parser-ast)
                              (map (fn [module] {:module module :open true}) default-imports)))
            name-resolution-ast
            (-> parser-ast
                (assoc :imports imports)
                (update :imports
                        (partial mapv
                                 (fn [{:keys [module] :as import}]
                                   (assoc import :module (db/->ref (:db/id (run module)))))))
                (update :imports not-empty)
                name-resolution/run)
            {db :db-after} (type-checker/run name-resolution-ast)
            ;; type-checker-ast (db/touch (db/->entity db eid))
            ;; {db :db-after} (desugar/run type-checker-ast)
            ]
        (db/touch (db/->entity db eid)))))

(comment

  (let [module {:ast/reference :module :name ["lang" "core"]}]
    (init)
    (run module)
    #_(db/touch (db/->entity @db/state [:name {:ast/reference :module :name ["lang" "option"]}])))


  (db/datoms @db/state)

  (db/->entity @db/state 85)


  (com.gfredericks.debug-repl/unbreak!!)


  )
