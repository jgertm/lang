(ns lang.name-resolution
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :as walk]
            [lang.ast :as ast]
            [lang.module :as module]
            [lang.utils :as utils :refer [undefined]]))

(defn- absorb-definition
  [module {:keys [name] :as definition}]
  (letfn [(qualify-internal-references [expr]
            (walk/prewalk
              (fn [node]
                (if (some-> node :reference #{:field :injector :constant})
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
      {:reference _ :in (source :guard (partial contains? modules))}
      (assoc node :in (get modules source))

      {:reference type}
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

(defn run
  [module]
  {:pre [(ast/module? module)]}
  (->> module
    :definitions
    (reduce
      (fn [module definition]
        (let [module     (absorb-definition module definition)
              definition (resolve-references module definition)]
          (update module :definitions conj definition)))
      (assoc module :definitions []))))
