(ns lang.name-resolution
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [lang.ast :as ast]
            [lang.definition :as definition]
            [lang.module :as module]
            [lang.pattern :as pattern]
            [lang.parser :as parser]
            [lang.state :refer [defquery]]
            [lang.term :as term]
            [lang.utils :as utils :refer [undefined]]
            [taoensso.timbre :as log]))

;; TODO: resolve OPENED modules' injectors
;; TODO: alert on UNKNOWN constructors

(defn- absorb-definition
  [module {:keys [name] :as definition}]
  (letfn [(qualify-internal-references [expr]
            (walk/prewalk
              (fn [node]
                (if (some-> node :ast/reference #{:field :injector :constant})
                  (assoc node :in (:name module))
                  node))
              expr))]
    (let [name (assoc name :in (:name module))]
      (match definition
        {:ast/definition :macro}
        (assoc-in module [:macros name] true)

        {:ast/definition :constant}
        (assoc-in module [:values name] true)

        {:ast/definition :type}
        (assoc-in module [:types name]
          (qualify-internal-references (:body definition)))

        {:ast/definition :typeclass :fields fields}
        (assoc-in module [:typeclasses name :fields]
          (qualify-internal-references fields))

        :else module))))

(defn- resolve-reference
  [module node]
  (let [modules
        (->> module
          :imports
          (map (juxt :alias :name ))
          (filter first)
          (into {}))]
    (match node
      {:ast/reference _ :in (source :guard (partial contains? modules))}
      (assoc node :in (get modules source))

      {:ast/reference type}
      (if-let [[reference _] (module/find
                               ((case type
                                  :typeclass module/all-typeclasses
                                  :type      module/all-types
                                  :constant  module/all-bindings
                                  :injector  module/all-injectors
                                  :field     module/all-fields
                                  (constantly nil))
                                module) node)]
        reference
        node)

      :else node)))

(defn- resolve-references
  [module definition]
  (walk/prewalk (partial resolve-reference module) definition))

(defn annotate-captures
  ;; FIXME(tjgr): this is jank AF
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

(defn run
  [module]
  {:pre [(ast/module? module)]}
  (log/debug "resolving names" (definition/name module))
  (->> module
    :definitions

(defquery global-references [key]
  (log/debug "collecting global references" key)
  (let [module (parser/ast key)
        unqualified-builtins
        (->> module/builtins
             keys
             (map (fn [name] [(dissoc name :in) name]))
             (into {}))
        locals
        (->> module
             :definitions
             (map (fn [definition]
                    [(:name definition)
                     (assoc (:name definition) :in (:name module))]))
             (into {}))
        imported
        (->> module
             :imports
             (map
              (fn [{:keys [module alias]}]
                (->> (global-references [:module module])
                     (keep (fn [[local full]]
                             (when-not (:in local)
                               [(assoc local :in alias)
                                full])))
                     (into {}))))
             (reduce merge))]
    (merge module/builtins unqualified-builtins locals imported)))

(defquery ast [key]
  (log/debug "name-resolving ast" key)
  (let [ast (parser/ast key)
        references (global-references key)]
    (ast/map
     (fn [node] (get references node node))
     ast)))

(comment

  (throw (ex-info "foo" {}))

  (com.gfredericks.debug-repl/unbreak!!)

  (com.gfredericks.debug-repl/unbreak!)

  (filter (fn [[k _]] (re-matches #".*println.*" (str k)))
  (-> "examples/arithmetic.lang"
    (lang.compiler/run :until :name-resolution)
    (module/all-bindings)))

(-> "examples/alist.lang"
    (lang.compiler/run :until :name-resolution)
    module/all-typeclasses
    (module/get {:name "Eq", :ast/reference :typeclass}))

  (global-references [:file "examples/arithmetic.lang"])


  (ast [:file "examples/arithmetic.lang"])


  (parser/ast [:file "examples/arithmetic.lang"])



  (macroexpand-1  '(defquery global-references [key]
     (log/debug "collecting global references" key)
     (let [ast (parser/ast key)]
       (map :name (:definitions ast)))))


  )
