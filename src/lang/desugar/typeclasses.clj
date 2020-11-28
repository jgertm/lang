(ns lang.desugar.typeclasses
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [lang.module :as module]
            [lang.term :as term]
            [lang.type :as type]
            [lang.utils :as utils :refer [undefined]]))

(defn- dictionary-type-name
  "Returns the name of a typeclasses' dictionary type."
  [name]
  (-> name
    (assoc :reference :type)
    (update :name #(format "D:%s" %))))

(defn- instance-value-name
  "Returns the name of an instances' dictionary value."
  [module typeclass types]
  {:pre [(every? type/is? types)]}
  {:reference :constant
   :name (format "I:%s:%s"
           (:name typeclass)
           (str/join "-" (mapv type/print types)))
   :in (:name module)})

(defn- dictionary-argument-name
  "Returns the name of a dictionary as a function argument."
  [typeclass]
  {:reference :dictionary
   :name      (gensym (:name typeclass))})

(defn add-dictionary-instance
  [module constraint name]
  (swap! (:desugar.typeclasses/dictionary-instances module) assoc constraint name))

(defn add-dictionary-argument
  [module constraint name]
  (swap! (:desugar.typeclasses/dictionary-arguments module) assoc constraint name))

(defn lookup-dictionary-argument
  [module constraint]
  (let [dictionary
        (or
          (get @(:desugar.typeclasses/dictionary-arguments module) constraint)
          (get @(:desugar.typeclasses/dictionary-instances module) constraint)

          (undefined ::lookup-dictionary-argument.chain)

          [constraint
           @(:desugar.typeclasses/dictionary-arguments module)
           @(:type-checker/instance-chains module)]

          (->> constraint
            (get @(:type-checker/instance-chains module))
            #_(reverse)
            #_(map (partial get (module/all-typeclass-dictionaries module))))

          constraint

          )]
    dictionary))

(defn- introduce-dictionary-arguments
  "Adds arguments for typeclass dictionaries to lambdas, according to
  the constraints in their type."
  [module definition]
  (letfn [(nonterminal? [node]
            (not ((some-fn term/symbol? term/atom?) node)))]
    (let [constraints (and (nonterminal? definition)
                           (type/constraints (term/type definition)))]
      (let [constrained-parameters
            (->>
              (for [{:keys [parameters] :as constraint} constraints
                    parameter                           parameters]
                [parameter constraint])
              (into {}))]
        (walk/postwalk
          (fn [node]
            (if-let [constraint
                     (and
                       (term/lambda? node)
                       (get constrained-parameters (:domain (term/type node))))]
              (let [{:keys [typeclass]} constraint
                    argument            (dictionary-argument-name typeclass)
                    type                {:ast/type :named :name (dictionary-type-name typeclass)}]
                (add-dictionary-argument module
                  constraint
                  {:ast/term               :symbol
                   :symbol                 argument
                   :type-checker.term/type type})
                {:ast/term :lambda
                 :argument argument
                 :body     node
                 :type-checker.term/type
                 {:ast/type :function
                  :domain   type
                  :return   (:type-checker.term/type node)}})
              node))
          definition)))))

(defn- pass-dictionary-arguments
  "Inserts a typeclass dictionary from scope into the arguments where
  expected according to the function's type."
  [module definition]
  (walk/postwalk
    (fn [{:keys [function] :as node}]
      (if-let [constraints (and (term/application? node)
                                (type/constraints (term/type function)))]
        (update node :arguments
          (fn [arguments]
            (into
              (mapv (fn [constraint]
                      (lookup-dictionary-argument module constraint)) constraints)
              arguments)))
        node))
    definition))

(defn- inline-typeclass-members
  "Replaces calls to typeclass member functions to accesses to the
  corresponding dictionaries."
  [module definition]
  (letfn [(only [coll]
            (if (= 1 (count coll))
              (first coll)
              nil))
          (typeclass-member [{:keys [symbol] :as function}]
            (when-let [{:keys [typeclass] :as constraint}
                       (only (type/constraints (term/type function)))]
              (let [{:keys [fields]}
                    (-> module
                      (module/all-typeclasses)
                      (module/get typeclass))]
                (find fields symbol))))]
    (walk/postwalk
      (fn [node]
        (if-let [[member type]
                 (and (term/application? node)
                      (typeclass-member (:function node)))]
          (let [field      (assoc member :reference :field)
                dictionary (first (:arguments node))]
            (-> node
              (update :arguments next)
              (assoc :function {:ast/term :extract
                                :record dictionary
                                :field field})))
          node))
      definition)))

(defn- desugar-term
  [module form]
  (->> form
    (introduce-dictionary-arguments module)
    (pass-dictionary-arguments module)
    (inline-typeclass-members module)))

(defn- desugar-declaration
  "Converts a typeclass declaration into a record type defintion."
  [module {:keys [name params fields] :as declaration}]
  (let [name (dictionary-type-name name)]
    ;; (swap! (:desugar.typeclasses/dictionary-types module) assoc (:name definition) name)
    {:ast/definition :type
     :name           name
     :params         params
     :body
     {:ast/type :record
      :fields   (->> fields
                  (mapv (fn [[name type]]
                          [(assoc name :reference :field) type]))
                  (into (empty fields)))}}))

(defn- desugar-instance
  "Converts a typeclass instance declaration into a global dictionary
  record value."
  [module {:keys [name types fields superclasses]}]
  (let [value-name (instance-value-name module name types)
        type-name  (dictionary-type-name name)
        type       {:ast/type   :application
                    :operator   {:ast/type :named :name type-name}
                    :parameters types}]
    (add-dictionary-instance module
      {:ast/constraint :instance
       :typeclass      name
       :parameters     types}
      {:ast/term :symbol :symbol value-name})
    (when superclasses (undefined ::superclasses))

    (comment
      fields

      )

    (->> superclasses
      (map (fn [[typeclass types]]
             {:ast/constraint :instance
              :typeclass typeclass
              :parameters types}))
      (reduce (fn [expr {:keys [typeclass] :as constraint}]
                (let [argument (dictionary-argument-name typeclass)]
                  (add-dictionary-argument module constraint argument)
                  {:ast/term :lambda
                   :argument argument
                   :body expr}))
        {:ast/definition :constant
         :name           value-name
         :body
         {:ast/term :record
          :fields   (->> fields
                      (map (fn [[field term]]
                             [(assoc field :reference :field)
                              (desugar-term module term)]))
                      (into (empty fields)))
          :type-checker.term/type
          type}}))

    ))

(defn desugar
  [module definition]
  (let [module (assoc module :desugar.typeclasses/dictionary-arguments (atom {}))]
    (match definition
      {:ast/definition :typeclass}
      (desugar-declaration module definition)

      {:ast/definition :typeclass-instance}
      (desugar-instance module definition)

      {:ast/definition :constant}
      (desugar-term module definition)

      _ definition)))

(comment

  (do (println "\n–-—")
      (-> "examples/option.lang"
        (lang.compiler/run :until :desugar)
        ;; :type-checker/instance-chains
        ;; deref
        :definitions
        first
        #_(nth 2)))


  )
