(ns lang.desugar.typeclasses
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [lang.module :as module]
            [lang.term :as term]
            [lang.type :as type]
            [lang.utils :as utils :refer [undefined]]))

(defn- dictionary-type
  "Returns the name of a typeclasses' dictionary type."
  [name]
  {:ast/type :named
   :name
   (-> name
     (assoc :reference :type)
     (update :name #(format "D:%s" %)))})

(defn- instance-value-name
  "Returns the name of an instances' dictionary value."
  [module typeclass types]
  {:pre [(every? type/is? types)]}
  {:reference :constant
   :name (format "I:%s:%s"
           (:name typeclass)
           (str/join "-" (map type/print types)))
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

(defn lookup-dictionary
  [module constraint]
  (or
    (get @(:desugar.typeclasses/dictionary-arguments module) constraint)
    (get @(:desugar.typeclasses/dictionary-instances module) constraint)
    (if-let [dictionaries
             (some->> constraint
               (get @(:type-checker/instance-chains module))
               (map (partial lookup-dictionary module)))]
      (reduce
        (fn [expr dictionary]
          {:ast/term :application
           :function expr
           :arguments [dictionary]})
        (first dictionaries)
        (next dictionaries))
      (undefined))))

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
                       (some 
                         #(when (type/contains? (term/type node) (key %)) (val %))
                         constrained-parameters))]
              (let [{:keys [typeclass]} constraint
                    argument            (dictionary-argument-name typeclass)
                    type                (dictionary-type typeclass)]
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
                      (lookup-dictionary module constraint)) constraints)
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
                                :record   dictionary
                                :field    field
                                :type-checker.term/type
                                (->> type
                                  (iterate :body)
                                  (some #(when-not (-> % :ast/type #{:forall :guarded}) %)))})))
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
  (let [{:keys [name]} (dictionary-type name)]
    ;; (swap! (:desugar.typeclasses/dictionary-types module) assoc (:name definition) name)
    {:ast/definition :type
     :name           name
     :params         params
     :body
     {:ast/type :record
      :fields   (->> fields
                  (map (fn [[name type]]
                         [(assoc name :reference :field) type]))
                  (into (empty fields)))}}))

(defn- desugar-instance
  "Converts a typeclass instance declaration into a global dictionary
  record value."
  [module {:keys [name types fields superclasses]}]
  (let [value-name (instance-value-name module name types)
        type       {:ast/type   :application
                    :operator   (dictionary-type name)
                    :parameters types}]
    (add-dictionary-instance module
      {:ast/constraint :instance
       :typeclass      name
       :parameters     types}
      {:ast/term :symbol :symbol value-name})
    {:ast/definition :constant
     :name           value-name
     :body
     (->> superclasses
       (reduce
         (fn [expr {:keys [typeclass parameters] :as constraint}]
           (let [argument (dictionary-argument-name typeclass)
                 type     {:ast/type   :application
                           :operator   (dictionary-type typeclass)
                           :parameters parameters}]
             (add-dictionary-argument module constraint
               {:ast/term :symbol :symbol argument})
             {:ast/term :lambda
              :argument argument
              :body     expr
              :type-checker.term/type
              {:ast/type :function
               :domain   type
               :return   (:type-checker.term/type expr)}}))
         {:ast/term :record
          :fields   (->> fields
                      (map (fn [[field term]]
                             [(assoc field :reference :field)
                              term]))
                      (into (empty fields)))
          :type-checker.term/type
          type})
       (pass-dictionary-arguments module)
       (inline-typeclass-members module))}))

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
