(ns lang.desugar
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [lang.utils :as utils :refer [undefined]]
            [lang.module :as module]
            [lang.term :as term]
            [lang.type :as type]))

(defn- desugar-macros
  [definition]
  (walk/prewalk
    (fn [node]
      (if-let [expansion (:type-checker.macro/expands-to node)]
        (recur expansion)
        node))
    definition))

(defn- typeclass-record-name
  [name]
  (-> name
    (assoc :reference :type)
    (update :name #(format "D:%s" %))))

(defn- desugar-typeclasses
  [module definition]
  (match definition
    {:ast/definition :typeclass}
    (let [name (typeclass-record-name (:name definition))]
      (swap! (:desugar.typeclasses/dictionary-types module) assoc (:name definition) name)
      {:ast/definition :type
       :name           name
       :params         (:params definition)
       :body           {:ast/type :record
                        :fields   (->> definition
                                    :fields
                                    (mapv (fn [[name type]]
                                            [(assoc name :reference :field) type]))
                                    (into (empty (:fields definition))))}})

    {:ast/definition :typeclass-instance}
    (let [types (->> definition
                  :types
                  (map (comp :name :name))
                  (str/join "-"))
          name  (-> definition
                  :name
                  (assoc :reference :variable)
                  (update :name #(format "I:%s:%s" % types)))]
      (swap! (:desugar.typeclasses/dictionary-instances module) assoc
        ((juxt :name :types) definition)
        name)
      {:ast/definition :constant
       :name           name
       :body
       {:ast/term :record
        :fields   (->> definition
                    :fields
                    (map (fn [[name term]]
                           [(assoc name :reference :field) term]))
                    (into (empty (:fields definition))))
        :type-checker.term/type
        {:ast/type   :application
         :operator   {:ast/type :named :name (typeclass-record-name (:name definition))}
         :parameters (:types definition)}}})

    {:ast/definition :constant}
    (let [dictionary-arguments (atom {})]
      (letfn [(get-dictionary-argument [constraint]
                (get
                  (merge
                    @(:desugar.typeclasses/dictionary-instances module)
                    @dictionary-arguments)
                  ((juxt :typeclass :parameters) constraint)))
              (add-dictionary-arguments [term]
                (walk/postwalk
                  (fn [node]
                    (if-let [constraints
                             (and
                               (term/lambda? node)
                               (type/constraints (:type-checker.term/type node)))]
                      (reduce
                        (fn [term {:keys [typeclass parameters] :as constraint}]
                          (let [argument      {:reference :variable :name (gensym (:name typeclass))}
                                argument-type {:ast/type   :application
                                               :operator   (get @(:desugar.typeclasses/dictionary-types module) typeclass)
                                               :parameters parameters}]
                            (swap! dictionary-arguments assoc
                              ((juxt :typeclass :parameters) constraint)
                              argument)
                            (letfn [(fix-type [type constraint]
                                      (walk/postwalk
                                        (fn [node]
                                          (match node
                                            {:ast/type :guarded :proposition constraint :body body}
                                            {:ast/type :function
                                             :domain   argument-type
                                             :return   body}

                                            _ node))
                                        type))]
                              {:ast/term :lambda
                               :argument argument
                               :body     (update term :type-checker.term/type #(-> % :body :body))
                               :type-checker.term/type
                               (fix-type (:type-checker.term/type term) constraint)})))
                        node
                        constraints)
                      node))
                  term))
              (pass-dictionaries [term]
                (walk/postwalk
                  (fn [node]
                    (if-let [constraints
                             (and
                               (term/application? node)
                               (type/constraints (:type-checker.term/type (:function node))))]
                      (update node :arguments
                        (fn [arguments]
                          (into (mapv get-dictionary-argument constraints) arguments)))
                      node))
                  term))
              (inline-typeclass-fields [term]
                (walk/postwalk
                  (fn [node]
                    (let [function (:function node)]
                      (or
                        (when-let [{:keys [typeclass parameters] :as constraint}
                                   (and
                                     (term/application? node)
                                     (term/symbol? function)
                                     (first (type/constraints (:type-checker.term/type function))))]
                          (when (contains?
                                  (-> module
                                    (module/all-typeclasses)
                                    (module/get (:typeclass constraint))
                                    :fields)
                                  (:symbol function))
                            (let [argument-type
                                  {:ast/type   :application
                                   :operator   (get @(:desugar.typeclasses/dictionary-types module) typeclass)
                                   :parameters parameters}]
                              (-> node
                                (assoc :function
                                  (merge 
                                    {:ast/term :extract ; TODO: this AST node is moot. use pattern matching instead?
                                     :record   {:ast/term :symbol
                                                :symbol   (get-dictionary-argument constraint)
                                                :type-checker.term/type argument-type}
                                     :field    (assoc (:symbol function) :reference :field)
                                     :type-checker.term/type
                                     (-> function
                                       :type-checker.term/type
                                       :body
                                       :body)}))
                                (update :arguments (comp vec next))))))
                        node)))
                  term))
              (fix-recur-types [term]
                (walk/postwalk
                  (fn [node]
                    (match node
                      {:ast/term :recur :body body}
                      (merge
                        node
                        (select-keys body [:type-checker.term/type]))

                      _ node))
                  term))]
        (assoc definition :body
          (-> (:body definition)
            (add-dictionary-arguments)
            (pass-dictionaries)
            (inline-typeclass-fields)
            (fix-recur-types)))))

    _ definition))

(defn- desugar
  [module definition]
  (->> definition
    (desugar-macros)
    (desugar-typeclasses module)))

(defn- init
  [module]
  (merge module
    {:definitions                            []
     :desugar.typeclasses/dictionary-types   (atom {})
     :desugar.typeclasses/dictionary-instances (atom {})}))

(defn run
  [module]
  (reduce
    (fn [module definition]
      (update module :definitions conj (desugar module definition)))
    (init module)
    (:definitions module)))

(comment

  (do (println "\n–-—")
      (-> "std/lang/option.lang"
        (lang.compiler/run :until :type-checker)
        :definitions
        #_module/surface-bindings))

  )
