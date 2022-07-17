(ns lang.name-resolution
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [lang.ast :as ast]
            [lang.db :as db]
            [lang.definition :as definition]
            [lang.pattern :as pattern]
            [lang.term :as term]
            [lang.utils :as utils :refer [undefined]]
            [taoensso.timbre :as log]
            [lang.type :as type]))

;; TODO: resolve OPENED modules' injectors
;; TODO: alert on UNKNOWN constructors
;; FIXME(tjaeger): shadowing

(defn surface-symbols
  [module]
  (->> module
       :definitions
       (mapcat
        (fn [{:keys [body]
              kind :ast/definition
              :as definition}]
          (cond-> []
            (contains? #{:constant :type :macro :typeclass/declaration} kind)
            (conj definition)

            (= :type kind)
            (concat (keys (type/injectors body))
                    (keys (type/fields body)))

            (= :typeclass/declaration kind)
            (concat (:members definition)))))
       (map (fn [{:keys [db/id name]}]
              [(select-keys name [:ast/reference :name]) (db/->ref id)]))
       (into {})))

(defn imported-symbols
  [module]
  (let [db @db/state]
    (->> module
         :imports
         (mapcat
          (fn [{:keys [module alias]}]
            (->> module
                 (db/->entity db)
                 surface-symbols
                 (map (fn [[n r]]
                        [(cond-> n
                           alias (assoc :in alias)) r])))))
         (into {}))))

(comment
  (->> [:name {:name ["lang" "option"], :ast/reference :module}]
       db/->ref
       (db/->entity @db/state)
       imported-symbols)

  (->> [:name {:name ["lang" "core"], :ast/reference :module}]
       db/->ref
       (db/->entity @db/state)
       (db/touch)
       #_imported-symbols)

  )

;; (surface-symbols (db/->entity @db/state 10))

(defn annotate-captures
  ;; FIXME(tjgr): this is jank AF
  ;; TODO(tjaeger): move to code generator, this is setup for closure conversion
  [definition]
  (letfn [(restrict [node symbols]
            (cond
              (term/lambda? node)
              (reduce
               (fn [symbols argument]
                 (disj symbols (select-keys argument [:ast/reference :name])))
               symbols
               (:arguments node))

              (term/match? node)
              (->> node
                   :branches
                   (mapcat :patterns)
                   (mapcat pattern/nodes)
                   (keep :symbol)
                   (set)
                   (set/difference symbols))

              :else symbols))]
    (->> definition
         (walk/postwalk
          (fn [node]
            (cond
              (not (term/is? node)) node

              (and (term/symbol? node) (not (:in (:symbol node))))
              (assoc node :name-resolution/captured-symbols #{(:symbol node)})

              :else
              (->> node
                   (term/children)
                   (map :name-resolution/captured-symbols)
                   (reduce set/union)
                   (restrict node)
                   (assoc node :name-resolution/captured-symbols)))))
         (walk/prewalk
          (fn [node]
            (cond
              (not (term/is? node)) node

              (term/lambda? node)
              node

              :else
              (dissoc node :name-resolution/captured-symbols)))))))

(defn- canonicalize-definition-names
  [{:keys [db/id] :as module}]
  (let [names (->> module
                   :definitions
                   (map (fn [{:keys [name]}]
                          [name (assoc name :in (db/->ref id))]))
                   (into {}))
        module (update module :definitions
                       (partial mapv
                                (fn [definition]
                                  (cond-> definition
                                    (contains? #{:type :constant :macro :typeclass/declaration} (:ast/definition definition))
                                    (update :name #(get names % %))

                                    (= :typeclass/declaration (:ast/definition definition))
                                    (update :members (partial mapv (fn [member] (assoc-in member [:name :in] (db/->ref (:db/id definition))))))))))
        {:keys [db-after]}
        (db/tx! db/state [module]
                {:lang.compiler/pass ::canonicalize-definition-names})]
    (db/touch (db/->entity db-after id))))

(defn- reify-labels
  [{mid :db/id :as module}]
  (let [module
        (update module :definitions
                (partial mapv (fn [{did :db/id :as definition}]
                                (match definition
                                       {:ast/definition :type
                                        :body body}
                                       (let [injectors (keys (type/injectors body))
                                             fields (keys (type/fields body))]
                                         (update definition :body
                                                 (partial walk/postwalk-replace
                                                          (->> (concat injectors fields)
                                                               (map (fn [l]
                                                                      [l
                                                                       {:db/id (gensym)
                                                                        :name (assoc l :in (db/->ref mid))
                                                                        :belongs-to (db/->ref did)}]))
                                                               (into {})))))

                                       _ definition))))
        {:keys [db-after]}
        (db/tx! db/state [module]
                {:lang.compiler/pass ::reify-labels})]
    (db/touch (db/->entity db-after mid))))

(defn- resolve-syntax-references
  [{:keys [db/id] :as module}]
  (letfn [(reify-bindings [node key]
            (let [tempids (repeatedly gensym)]
              [(zipmap (get node key)
                       (map db/->ref tempids))
               (update node key (partial mapv (fn [id arg] (assoc arg :db/id id)) tempids))]))]
    (let [references (merge (surface-symbols module)
                            (imported-symbols module))
          module (update module :definitions
                         (partial mapv
                                  (fn [definition]
                                    (ast/walk
                                     (fn [references node]
                                       (match node
                                              {:ast/definition (:or :type :typeclass/declaration)}
                                              (-> node
                                                  (reify-bindings :params)
                                                  (update 0 (partial merge references)))

                                              {:ast/definition :macro}
                                              (-> node
                                                  (reify-bindings :arguments)
                                                  (update 0 (partial merge references)))

                                              {:ast/term :lambda}
                                              (-> node
                                                  (reify-bindings :arguments)
                                                  (update 0 (partial merge references)))

                                              {:ast/term :branch}
                                              (let [symbols (->> (get node :patterns)
                                                                 (mapcat pattern/nodes)
                                                                 (filter pattern/symbol?)
                                                                 (map :symbol))
                                                    tempids (zipmap symbols (map db/->ref (repeatedly gensym)))]
                                                [(merge references tempids)
                                                 (update node :patterns
                                                         (partial walk/postwalk
                                                                  (fn [node]
                                                                    (if-let [id (:tempid (get tempids node))]
                                                                      (assoc node :db/id id)
                                                                      node))))])


                                              _ [references (get references node node)]))
                                     references
                                     definition))))
          {:keys [db-after]}
          (db/tx! db/state [module]
                  {:lang.compiler/pass ::resolve-syntax-references})]
      (db/touch (db/->entity db-after id)))))

(defn run
  [module]
  {:pre [(ast/module? module)]}
  (log/debug "resolving names" (definition/name module))
  (-> module
      canonicalize-definition-names
      reify-labels
      resolve-syntax-references))

(comment
  (let [module {:ast/reference :module :name ["lang" "option"]}]
    (#'lang.compiler/init)
    (#'lang.compiler/run module))

  (db/datoms @db/state)

  (reset! db/state (db/init))

  (db/->entity @db/state [:name {:ast/reference :module
                                 :name ["lang" "native" "jvm"]}])


  (throw (ex-info "foo" {}))

  (com.gfredericks.debug-repl/unbreak!!)

  (com.gfredericks.debug-repl/unbreak!)


  (-> "examples/alist.lang"
      (lang.compiler/run :until :name-resolution)
      #_module/all-typeclasses
      #_(module/get {:name "Eq", :ast/reference :typeclass}))

  )
