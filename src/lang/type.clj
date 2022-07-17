(ns lang.type
  (:refer-clojure :exclude [contains? print])
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn is?
  [form]
  (and (associative? form) (:ast/type form)))

(defn universally-quantified?
  [type]
  (match type
    {:ast/type :forall} true
    _ false))

(defn existentially-quantified?
  [type]
  (match type
    {:ast/type :exists} true
    _ false))

(defn quantified?
  [type]
  (or
    (universally-quantified? type)
    (existentially-quantified? type)))

(defn constraints
  "Collects outer constraints of the type. Skips over foralls."
  [type]
  (match type
    {:ast/type :forall :body body}
    (recur body)

    {:ast/type :guarded :proposition proposition :body body}
    (conj (constraints body) proposition)

    _ nil))

(defn primitive?
  [type]
  (match type
    {:ast/type :primitive} true
    _ false))

(defn existential-variable?
  [type]
  (match type
    {:ast/type :existential-variable} true
    _ false))

(defn universal-variable?
  [type]
  (match type
    {:ast/type :universal-variable} true
    _ false))

(defn function?
  [type]
  (match type
    {:ast/type :function} true
    _ false))

(defn polarity
  [type]
  (case (:ast/type type)
    :forall :negative
    :exists :positive
    :neutral))

(defn negative?
  [type]
  (= :negative (polarity type)))

(defn positive?
  [type]
  (= :positive (polarity type)))

(defn parameters
  [type]
  (match type
    {:ast/type :forall :variable variable :body body}
    (cons
      variable
      (parameters body))

    _ nil))

(defn injectors
  [type]
  (match type
    {:ast/type :variant :injectors injectors}
    injectors

    {:ast/type :forall :body body}
    (recur body)

    _ nil))

(defn fields
  [type]
  (match type
    {:ast/type :record :fields fields}
    fields

    {:ast/type :forall :body body}
    (recur body)

    _ nil))

(defn function
  [types]
  (->> types
    (reverse)
    (reduce
      (fn [acc type]
        {:ast/type :function
         :domain   type
         :return   acc}))))

(defn nodes
  [type]
  (letfn [(branch? [node] (is? node))
          (children [node]
            (match node
              {:ast/type :variant}
              (->> node
                :injectors
                (vals)
                (filter some?))

              {:ast/type :record}
              (vals (:fields node))

              {:ast/type :application :operator operator :parameters parameters}
              (cons operator parameters)

              _
              (->> node
                (vals)
                (filter is?))))]
    (tree-seq branch? children type)))

(defn contains?
  [type child]
  (some (partial = child) (nodes type)))

(defn free-variables
  [type]
  (let [nodes (nodes type)
        variables
        (->> nodes
          (filter #(-> % :ast/type (= :existential-variable)))
          (set))
        bound
        (->> nodes
          (keep #(match %
                   {:ast/type :forall :variable variable}
                   variable

                   _ nil))
          (set))]
    (set/difference variables bound)))

(defn free-existential-variables
  [type]
  (->> type
    (free-variables)
    (filter #(-> % :ast/type (= :existential-variable)))
    (set)))

(defn instantiate-universals
  [type instantiations]
  (match [type instantiations]
    [{:ast/type :forall :variable universal-variable :body body}
     [instantiation & more-instantiations]]
    (recur
      (walk/postwalk-replace {universal-variable instantiation} body)
      more-instantiations)

    [_ []]
    type))

(def greek-alphabet
  '("α" "β" "γ" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ"
    "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "ϕ" "χ" "ψ" "ω"))

(defn print
  ([type]
   (print
     (atom {:letters greek-alphabet
            :mapping {}})
     type))
  ([env type]
   (letfn [(collect-arguments [type]
             (match type
               {:ast/type :function :domain domain :return return}
               (cons domain (collect-arguments return))

               _ (list type)))
           (collect-quantifiers [type]
             (match type
               {:ast/type :forall :variable variable :body body}
               (merge-with cons
                 {:variables variable}
                 (collect-quantifiers body))

               _ {:inner     type
                  :variables '()}))
           (print-proposition [proposition]
             (match proposition
               {:ast/constraint :instance
                :typeclass      typeclass
                :parameters     parameters}
               (format "%s %s"
                 (:name typeclass)
                 (str/join " " (map (partial print env) parameters)))))]
     (match type
       {:ast/type :named :name name}
       (if true
         (:name name))

       {:ast/type :application :operator operator :parameters parameters}
       (format "(%s %s)"
         (print env operator)
         (str/join " " (mapv (partial print env) parameters)))

       {:ast/type :universal-variable :id id}
       (if-let [symbol (get-in @env [:mapping id])]
         symbol
         (let [new-symbol (peek (:letters @env))]
           (swap! env #(-> %
                         (update :letters pop)
                         (update :mapping assoc id new-symbol)))
           new-symbol))

       {:ast/type :quote :inner inner}
       (format "(Expr %s)" (print env inner))

       {:ast/type :function}
       (->> type
         (collect-arguments)
         (map (partial print env))
         (str/join " ")
         (format "(-> %s)"))

       {:ast/type :forall :variable variable :body body}
       (let [{:keys [inner variables]} (collect-quantifiers type)]
         (format "(∀ %s %s)"
           (str/join " " (map (partial print env) variables))
           (print inner)))

       {:ast/type    :guarded
        :proposition proposition
        :body        body}
       (format "(=> (%s) %s)"
         (print-proposition proposition)
         (print body))))))
