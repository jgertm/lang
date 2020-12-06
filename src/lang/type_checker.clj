(ns lang.type-checker
  (:refer-clojure :exclude [apply drop type])
  (:require [clojure.core.match :refer [match]]
            clojure.reflect
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [lang.ast :as ast]
            [lang.interpreter :as interpreter]
            [lang.jvm :as jvm]
            [lang.module :as module]
            [lang.term :as term]
            [lang.type :as type]
            [lang.utils :as utils :refer [undefined]]
            [lang.zip :as zip])
  (:import java.lang.Class))

(declare apply)
(declare analysis:check)
(declare match:check)
(declare synthesize)
(declare subtyping:join)
(declare subtyping:polarity)
(declare subtype)
(declare instantiate-to)

(defn- principality?
  [principality]
  (contains? #{:principal :non-principal} principality))

(defn- polarity?
  [polarity]
  (contains? #{:positive :negative :neutral} polarity))

(defn- bottom []
  (throw (ex-info "bottom" {})))

(defn- next-variable-id
  [module]
  (swap! (:type-checker/current-variable module) inc))

(defn- fresh-existential
  ([module]
   (fresh-existential module :kind/type))
  ([module kind]
   (let [variable-id (next-variable-id module)
         type    {:ast/type :existential-variable
                  :id       variable-id}]
     (swap! (:type-checker/facts module)
       zip/insert-left
       {:fact/declare-existential variable-id
        :kind                     kind})
     type)))

(defn- fresh-universal
  ([module]
   (fresh-universal module :kind/type))
  ([module kind]
   (let [variable-id (next-variable-id module)
         type        {:ast/type :universal-variable
                      :id       variable-id}]
     (swap! (:type-checker/facts module)
       zip/insert-left
       {:fact/declare-universal variable-id
        :kind                   kind})
     type)))

(defn- declare-universal
  [module {:keys [id] :as universal} kind]
  {:pre [(map? universal) (some? id)]}
  (swap! (:type-checker/facts module)
    zip/insert-left
    {:fact/declare-universal id
     :kind                   kind})
  nil)

(defn- fresh-mark
  [module]
  (let [marker {:fact/marker (next-variable-id module)}]
    (swap! (:type-checker/facts module) zip/insert-left marker)
    marker))

(defn- drop
  [module fact]
  (let [zipper
        (swap! (:type-checker/facts module)
          #(-> %
             (zip/->end)
             (zip/focus-left fact)
             (zip/left)))
        [remainder discard] (zip/split zipper)]
    (reset! (:type-checker/facts module) remainder)
    discard))

(defn- bind-symbol
  [module symbol type principality]
  (swap! (:type-checker/facts module)
    zip/insert-left
    {:fact/bind-symbol (select-keys symbol [:reference :name :in])
     :type             type
     :principality     principality})
  nil)

(defn- existential-facts
  [module]
  (->> module
    :type-checker/facts
    (deref)
    (zip/left-seq)
    (keep (fn [fact]
            (some-> fact
              ((some-fn :fact/solve-existential :fact/restrict-existential :fact/declare-existential))
              (vector fact))))
    (into {})))

(defn- existential-solutions
  [module]
  (->> module
    (existential-facts)
    (keep (fn [[k v]]
            (match v
              {:fact/solve-existential _}
              [k (:type v)]

              _ nil)))
    (into {})))

(defn- existential-restrictions
  [module]
  (->> module
    (existential-facts)
    (keep (fn [[k v]]
            (match v
              {:fact/restrict-existential _ :constraints constraints}
              {k constraints}

              _ nil)))
    (reduce (partial merge-with set/union))))

(defn- universal-facts
  [module]
  (->> module
    :type-checker/facts
    (deref)
    (zip/left-seq)
    (keep (fn [fact]
            (some-> fact
              ((some-fn :fact/declare-universal :fact/constrain-universal :fact/equate-universal))
              (vector fact))))))

(defn- universal-equations
  [module]
  (->> module
    (universal-facts)
    (keep (fn [[k v]]
            (match v
              {:fact/equate-universal _}
              [k (:type v)]

              _ nil)))
    (into {})))

(defn- universal-constraints
  [module]
  (->> module
    (universal-facts)
    (keep (fn [[k v]]
            (match v
              {:fact/constrain-universal _}
              {k (:constraints v)}

              _ nil)))
    (reduce (partial merge-with set/union))))

(defn- unsolved?
  [module existential-variable]
  (not
    (or
      (contains? (existential-solutions module) existential-variable)
      (contains? (existential-restrictions module) existential-variable))))

(defn- restricted?
  [module existential-variable]
  (contains? (existential-restrictions module) existential-variable))

(defn- equated?
  [module universal-variable]
  (contains? (universal-equations module) universal-variable))

(defn- constrained?
  [module universal-variable]
  (contains? (universal-constraints module) universal-variable))

(defn- local-bindings
  [module]
  (->> module
    :type-checker/facts
    (deref)
    (zip/left-seq)
    (keep (fn [fact]
            (when-let [symbol (:fact/bind-symbol fact)]
              [symbol ((juxt :type :principality) fact)])))
    (into {})))

(defn- all-bindings
  [module]
  (merge
    (module/all-bindings module)
    (local-bindings module)))

(defn- lookup-binding
  [module symbol]
  (or
    (when-let [[type principality]
               (some->>
                 (select-keys symbol [:reference :name :in])
                 (module/get (all-bindings module)))]
      [(apply module type) principality])
    (throw (ex-info "Unknown binding"
             {:symbol symbol
              :module (:name module)}))))

(defn- lookup-type
  [module name]
  (let [local-name   name
        types        (module/all-types module)]
    (or
      (module/get types local-name)
      (undefined ::lookup-type)
      (throw (ex-info "Unknown type"
               {:type     name
                :module   (:name module)
                :in-scope (keys types)})))))

(defn- lookup-macro
  [module name]
  (module/get (module/all-macros module) name))

(defn- apply
  [module type]
  (match type
    {:ast/type :existential-variable :id id}
    (or
      (some->> id
        (get (existential-solutions module))
        (apply module))
      type)

    {:ast/type :universal-variable :id id}
    (or
      (some->> id
        (get (universal-equations module))
        (apply module))
      type)

    {:ast/type :forall}
    (letfn [(unwrap-nonuniversal [type]
              (match type
                {:ast/type :forall :variable variable :body body}
                (if-not (type/universal-variable? variable)
                  body
                  type)

                _ type))]
      (-> type
        (update :variable (partial apply module))
        (update :body (partial apply module))
        (unwrap-nonuniversal)))

    {:ast/type :guarded}
    (-> type
      (update-in [:proposition :parameters] (partial mapv (partial apply module)))
      (update :body (partial apply module)))

    {:ast/type :application}
    (-> type
      (update :operator (partial apply module))
      (update :parameters (partial mapv #(apply module %))))

    {:ast/type :function}
    (-> type
      (update :domain (partial apply module))
      (update :return (partial apply module)))

    {:ast/type :variant}
    (update type :injectors
      #(->> %
         (map (fn [[injector value]] [injector (some->> value (apply module))]))
         (into (empty %))))

    {:ast/type :record}
    (update type :fields
      #(->> %
         (map (fn [[field content]] [field (apply module content)]))
         (into (empty %))))

    {:ast/type :primitive}
    type

    {:ast/type :named :name name}
    type

    {:ast/type (:or :object :method)}
    type

    {:ast/type :quote}
    (update type :inner (partial apply module))

    _
    (undefined :apply/fallthrough)))

(defn- solve-existential
  ([module existential solution]
   (solve-existential module existential solution :kind/type))
  ([module existential solution kind]
   {:pre [(int? existential) (map? solution)]}
   (let [current-fact (get (existential-facts module) existential)
         new-fact
         {:fact/solve-existential existential
          :kind                   kind
          :type                   (apply module solution)}]
     (swap! (:type-checker/facts module)
       #(walk/prewalk-replace           ; FIXME
          {current-fact new-fact}
          %))
     nil)))

(defn- restrict-existential
  ([module existential constraints]
   (restrict-existential module existential constraints :kind/type))
  ([module existential constraints kind]
   {:pre [(int? existential) (set? constraints)]}
   (let [current-fact (get (existential-facts module) existential)
         restrictions (get (existential-restrictions module) existential)
         new-fact
         {:fact/restrict-existential existential
          :kind                      kind
          :constraints               (set/union restrictions constraints)}]
     (swap! (:type-checker/facts module)
       #(walk/prewalk-replace
          {current-fact new-fact}
          %))
     nil)))

(defn- equate-universal
  [module universal solution]
  {:pre [(int? universal) (type/is? solution)]}
  (swap! (:type-checker/facts module)
    zip/insert-left
    {:fact/equate-universal universal
     :type solution})
  nil)

(defn- constrain-universal
  [module universal constraints]
  {:pre [(int? universal) (set? constraints)]}
  ;; TODO: assert no equation
  (swap! (:type-checker/facts module)
    zip/insert-left
    {:fact/constrain-universal universal
     :constraints constraints})
  nil)

(defn- clear-existential
  [module existential]
  {:pre [(int? existential)]}
  (swap! (:type-checker/facts module)
    #(walk/prewalk
       (fn [node]
         (match node
           {:fact/solve-existential existential}
           (merge
             {:fact/declare-existential existential}
             (select-keys node [:kind]))

           {:fact/restrict-existential existential}
           (merge
             {:fact/declare-existential existential}
             (select-keys node [:kind]))

           _ node))
       %))
  nil)

(defn- find-variant
  [module injector]
  (let [type
        (-> module
          (module/all-injectors)
          (module/get injector))
        parameters (->> type
                     :name
                     (lookup-type module)
                     (type/parameters)
                     (mapv (fn [_] (fresh-existential module))))]
    (if (seq parameters)
      {:ast/type   :application
       :operator   type
       :parameters parameters}
      type)))

(defn- setup-annotations
  [form]
  (walk/prewalk
    (fn [node]
      (cond-> node
        (:ast/term node)
        (assoc :type-checker.term/type (promise))

        (:ast/pattern node)
        (assoc :type-checker.pattern/type (promise))

        (= :application (:ast/term node))
        (assoc :type-checker.macro/expands-to (promise))))
    form))

(defn- resolve-annotations
  [module definition]
  (let [annotations [:type-checker.macro/expands-to
                     :type-checker.term/type
                     :type-checker.pattern/type]]
    (walk/prewalk
      (fn resolve-node [node]
        (if-not (map? node)
          node
          (merge
            (clojure.core/apply dissoc node annotations)
            (->> annotations
              (select-keys node)
              (keep (fn [[k promise]]
                      (when (realized? promise)
                        (let [value (deref promise)]
                          [k (cond->> value
                               (:ast/type value) (apply module))]))))
              (into (empty node))))))
      definition)))

(defn- annotate-term
  [term type]
  (when-let [promise (:type-checker.term/type term)]
    (deliver promise type)))

(defn- annotate-pattern
  [pattern type]
  (when-let [promise (:type-checker.pattern/type pattern)]
    (deliver promise type)))

(defn- generalize
  [module mark type]
  (let [type    (apply module type)
        discard (drop module mark)
        universal-variables
        (reduce
          (fn [universal-variables
               {:fact/keys [declare-existential restrict-existential]
                :keys      [kind constraints]
                :as        fact}]
            (let [existential-variable-id (or declare-existential restrict-existential)
                  existential-variable    {:ast/type :existential-variable
                                           :id       existential-variable-id}]
              (if (and
                    (some? existential-variable-id)
                    (type/contains? type existential-variable))
                (let [universal-variable (fresh-universal module)
                      constraints        (walk/prewalk-replace
                                           {existential-variable universal-variable}
                                           constraints)]
                  (swap! (:type-checker/facts module)
                    #(-> %
                       (zip/insert-right {:fact/solve-existential existential-variable-id
                                          :kind                   kind
                                          :type                   universal-variable})
                       (zip/right)))
                  (conj universal-variables [universal-variable (or constraints :unrestricted)]))
                (do (swap! (:type-checker/facts module)
                      #(-> %
                         (zip/insert-right fact)
                         (zip/right)))
                    universal-variables))))
          []
          discard)
        generalized-type
        (->> universal-variables
          (reverse)
          (reduce
            (fn [type quantification]
              (match quantification
                [universal-variable :unrestricted]
                {:ast/type :forall
                 :variable universal-variable
                 :body     type}

                [universal-variable constraints]
                (let [proposition (if (= 1 (count constraints)) ; FIXME: multiple constraints
                                    (first constraints)
                                    (undefined ::multiple-constraints))]
                  {:ast/type :forall
                   :variable universal-variable
                   :body     {:ast/type    :guarded
                              :proposition proposition
                              :body        type}})))
            type)
          (apply module))]
    generalized-type))

(defn- synthesize-atom
  [atom]
  (or
    (module/get
      (->> module/builtins
        (map (fn [[k v]] [(:primitive v) {:ast/type :named :name k}]))
        (into (empty module/builtins)))
      (:atom atom))
    (throw (ex-info "Unrecognized atom" {:atom atom}))))

(defn- incompatible-head-constructors?
  [type-1 type-2]
  (not= (:ast/type type-1) (:ast/type type-2)))

(defn- unify
  [module type-a type-b kind]
  (when-not (and
              (contains? #{:universal-variable :primitive :named} (:ast/type type-a))
              (= type-a type-b)) ; ElimeqUvarRefl ElimeqUnit
    (match [type-a type-b]
      [{:ast/type :universal-variable :id alpha} _]
      (cond
        (and
          (not (contains? (type/free-variables type-b) type-a))
          (constrained? module alpha))
        (undefined ::unify.uvar-l.constrained)

        (and
          (not (contains? (type/free-variables type-b) type-a))
          (not (equated? module alpha))) ; ElimeqUvarL
        (equate-universal module alpha type-b)

        (and
          (not= type-a type-b)
          (contains? (type/free-variables type-b) type-a)) ; ElimeqUvarL⊥
        (bottom))

      [_ {:ast/type :universal-variable :id alpha}]
      (cond
        (and
          (not (contains? (type/free-variables type-a) type-b))
          (constrained? module alpha))
        (undefined ::unify.uvar-r.constrained)

        (and
          (not (contains? (type/free-variables type-a) type-b))
          (not (equated? module alpha))) ; ElimeqUvarR
        (equate-universal module alpha type-a)

        (and
          (not= type-b type-a)
          (contains? (type/free-variables type-a) type-b)) ; ElimeqUvarR⊥
        (bottom))

      [{:ast/type :function :domain domain-1 :return return-1}
       {:ast/type :function :domain domain-2 :return return-2}] ; ElimeqBinF
      (do (unify module domain-1 domain-2 kind)
          (unify module (apply module return-1) (apply module return-2) kind))

      [{:ast/type :record} {:ast/type :record}] ; ElimeqBinR
      (undefined ::unify.record)

      [{:ast/type :variant} {:ast/type :variant}] ; ElimeqBinV
      (undefined ::unify.variant)

      [{:ast/type :named :name name-1}
       {:ast/type :named :name name-2}]
      (when-not (= name-1 name-2)
        (bottom))

      [{:ast/type :application :operator operator-1 :parameters parameters-1}
       {:ast/type :application :operator operator-2 :parameters parameters-2}]
      (do (unify module operator-1 operator-2 kind)
          (->>
            (map vector parameters-1 parameters-2)
            (run! (fn [[param-1 param-2]] (unify module param-1 param-2 kind)))))

      [_ _] ; ElimeqClash
      (if (incompatible-head-constructors? type-a type-b)
        (bottom)
        (undefined ::unify.fallthrough)))))

(defn- proposition:instance
  [module proposition]
  (letfn [(init [{:keys [variables] :as instance}]
            (run!
              #(declare-universal module % :kind/type)
              (vals variables))
            instance)
          (match-head [{:keys [parameters] :as proposition}
                       {:keys [superclasses types] :as instance}]
            (when
                (->>
                  (map vector types parameters)
                  (every? (fn [[type parameter]]
                            (unify module type (apply module parameter) :kind/type)
                            true)))
              [(assoc proposition :parameters types) superclasses]))
          (match-superclasses [[instance superclasses]]
            (->> superclasses
              (mapcat (partial proposition:instance module))
              (cons instance)
              (doall)))
          (find-chain [proposition instance]
            (let [mark (fresh-mark module)]
              (try
                (->> instance
                  (init)
                  (match-head proposition)
                  (match-superclasses))
                (catch Exception e
                  (drop module mark)
                  nil))))]
    (let [proposition (update proposition :parameters #(mapv (partial apply module) %))]
      (match proposition
        {:ast/constraint :instance :typeclass typeclass :parameters parameters}
        (let [{:keys [instances superclasses] :as typeclass}
              (-> module
                (module/all-typeclasses)
                (module/get typeclass))
              instance-chain
              (if (some (some-fn type/existential-variable? type/universal-variable?) parameters)
                (do
                  (run!
                    (fn [param]
                      (match param
                        {:ast/type :existential-variable :id alpha}
                        (restrict-existential module alpha #{proposition})

                        {:ast/type :universal-variable :id alpha}
                        (constrain-universal module alpha #{proposition}))
                      true)
                    parameters)
                  (list proposition))
                (some
                  (fn [instance] (find-chain proposition instance))
                  instances))]
          (when-not instance-chain
            (throw (ex-info "Missing instance"
                     {:typeclass  typeclass
                      :parameters parameters})))
          (when (not-empty instance-chain)
            (swap! (:type-checker/instance-chains module)
              assoc proposition instance-chain))
          instance-chain)))))

(defn- proposition:true
  "Γ ⊢ P true ⊣ Δ
  found on pg. 40"
  [module proposition]
  (match proposition
    {:ast/constraint :instance}
    (proposition:instance module proposition)

    _
    (undefined ::proposition:true)))

(defn- proposition:incorporate
  "Γ / P ⊣ Δ^⊥
  found on pg. 40"
  [module proposition]
  (match proposition
    {:ast/constraint :instance}
    (proposition:instance module proposition)

    _ (undefined ::proposition:incorporate.fallthrough)))

(defn- proposition:equivalent
  [module proposition-1 proposition-2]
  (undefined ::proposition:equivalent))

(defn- checked-intro-form?
  "chk-I
  found on pg. 10"
  [term]
  (match term
    {:ast/term :lambda} true
    {:ast/term :atom} true
    {:ast/term :record} true
    {:ast/term :variant} true
    _ false))

(defn- analysis:check
  "Γ ⊢ e ⇐ A p ⊣ Δ
  found on pg. 37"
  [module term [type principality]]
  {:pre [(term/is? term)
         (type/is? type) principality? principality?]}
  (let [type (apply module type)]
    (annotate-term term type)
    (match [term type principality]
      [{:ast/term :recur :reference reference :body body}
       _ _] ; Rec
      (let [marker (fresh-mark module)]
        (bind-symbol module reference type principality)
        (analysis:check module body [type principality])
        ;; FIXME: paper says to, but we don't drop here, otherwise the resolved type annotations on typeclass member functions will contain original universals
        #_(drop module marker))  

      [{:ast/term :atom :atom atom}
       {:ast/type :existential-variable :id alpha}
       :non-principal] ; 1Iα^
      (let [type (synthesize-atom atom)]
        (solve-existential module alpha type))

      [{:ast/term :atom :atom atom}
       {:ast/type :primitive :primitive primitive}
       _] ; 1I
      (when-not (= (:atom atom) primitive)
        (throw (ex-info "Distinct types" {:atom atom :primitive primitive})))

      [(_ :guard checked-intro-form?)
       {:ast/type :forall :variable alpha :body body}
       _] ; ∀I
      (do (declare-universal module alpha :kind/type)
          (analysis:check module term [body principality]))

      [(_ :guard checked-intro-form?)
       {:ast/type :guarded :proposition proposition :body body}
       :principal] ; ⊃I / ⊃I⊥
      (let [mark (fresh-mark module)]
        (proposition:incorporate module proposition)
        (analysis:check module term [(apply module body) :principal])
        nil)

      [{:ast/term :lambda :argument argument :body body}
       {:ast/type :function :domain domain :return return}
       _] ; →I
      (do (bind-symbol module argument domain principality)
          (analysis:check module body [return principality]))

      [{:ast/term :lambda :argument argument :body body}
       ({:ast/type :existential-variable :id alpha} :as alpha-type)
       :non-principal] ; →Iα^
      (let [current  (zip/node @(:type-checker/facts module))
            _        (swap! (:type-checker/facts module)
                       zip/focus-left
                       {:fact/declare-existential alpha :kind :kind/type})
            alpha-1  (or
                       (some->> argument :type (apply module))
                       (fresh-existential module))
            alpha-2  (fresh-existential module)
            function {:ast/type :function
                      :domain   alpha-1
                      :return   alpha-2}]
        (solve-existential module alpha function)
        (swap! (:type-checker/facts module) zip/focus-right current)
        (analysis:check module term [function :non-principal]))

      [{:ast/term :match :body body :branches branches} _ _] ; Case
      (let [[pattern-type pattern-principality] (synthesize module body)]
        (match:check module
          branches
          [[pattern-type] pattern-principality]
          [type principality])
        nil)

      [{:ast/term :variant :variant {:injector injector :value value}}
       {:ast/type :variant :injectors injectors}
       _] ; +I
      (cond
        (and (some? value) (some? (get injectors injector))) ; variant wraps value
        (analysis:check module value [(get injectors injector) principality])

        (and (nil? value) (contains? injectors injector)) ; variant is enum
        nil

        :default ; injector isn't defined
        (throw (ex-info "Unknown injector" {:injector injector})))

      [{:ast/term :variant :variant {:injector injector}}
       ({:ast/type :existential-variable :id alpha} :as existential-type)
       _] ; +Iα^
      (let [current (zip/node @(:type-checker/facts module))
            _       (swap! (:type-checker/facts module)
                      zip/focus-left
                      (get (existential-facts module) alpha))
            type
            (find-variant module injector)]
        (swap! (:type-checker/facts module) zip/focus-right current)
        (instantiate-to module existential-type [type :kind/type])
        (analysis:check module term [type principality]))

      [{:ast/term :record :fields fields}
       {:ast/type :record :fields rows}
       _] ; ×I
      (cond
        (= (keys fields) (keys rows))
        (run! (fn [[field value]]
                (let [row-type (get rows field)]
                  (analysis:check module value [row-type principality]))) fields)

        :else
        (undefined ::analysis:check.record-type)) 

      [{:ast/term :record}
       {:ast/type :existential-variable}
       _] ; ×Iα^
      (undefined ::analysis:check.record-exvar) 

      [_
       {:ast/type :named :name name}
       _]
      (let [type* (lookup-type module name)]
        (analysis:check module term [type* principality]))

      [_
       {:ast/type   :application
        :operator   ({:ast/type :named} :as operator)
        :parameters parameters}
       _]
      (match operator
        {:ast/type :named :name name}
        (let [type* (-> module
                      (lookup-type name)
                      (type/instantiate-universals parameters))]
          (analysis:check module term [type* principality]))

        {:ast/type :existential-variable}
        (do #_(undefined ::analysis:check.application.exvar)
            nil))

      [{:ast/term :sequence :operations operations} _ _]
      (let [return (last operations)]
        (->> operations
          (butlast)
          (run!
            (fn [operation]
              (let [alpha (fresh-existential module)]
                (analysis:check module
                  operation
                  [alpha :non-principal])))))
        (analysis:check module return [type principality]))

      [_ _ _] ; Sub
      (let [[synth-type] (synthesize module term)
            polarity     (subtyping:join
                           (subtyping:polarity type)
                           (subtyping:polarity synth-type))]
        (subtype module polarity synth-type type)))))

(defn- subtyping:join
  [type-a type-b]
  (match [type-a type-b]
    [:positive _] :positive
    [:negative _] :negative
    [:neutral :neutral] :negative
    [:neutral polarity] polarity))

(defn- subtyping:polarity
  [type]
  (match type
    {:ast/type :forall} :negative
    {:ast/type :exists} :positive
    _ :neutral))

(defn- before?
  [module & vars]
  (let [get-existential
        (some-fn
          :fact/declare-existential
          :fact/restrict-existential
          :fact/solve-existential)]
    (->> module
      :type-checker/facts
      (deref)
      (zip/left-seq)
      (reverse)
      (keep (comp (set vars) get-existential))
      (= vars))))

(defn- instantiate-to
  "Γ ⊢ α^ := t : κ ⊣ Δ
  found on pg. 43"
  [module type [solution kind]]
  (match [type [solution kind]]
    [({:ast/type :existential-variable :id alpha} :as alpha-type)
     [({:ast/type :existential-variable :id beta} :as beta-type) _]]
    (cond
      (and (unsolved? module beta) (before? module alpha beta)) ; InstReach
      (solve-existential module beta alpha-type kind)

      (and (restricted? module beta) (before? module alpha beta)) ; InstReach⊃
      (do (restrict-existential module alpha (get (existential-restrictions module) beta))
          (clear-existential module beta)
          (solve-existential module beta alpha-type kind))

      :else ; InstSolve
      (solve-existential module alpha beta-type kind))

    [{:ast/type :existential-variable :id alpha}
     [{:ast/type :variant :injectors injectors} _]] ; InstBin
    (let [_ (swap! (:type-checker/facts module)
              zip/focus-left
              {:fact/declare-existential alpha :kind :kind/type})
          injectors*
          (->> injectors
            (map (fn [[injector value]]
                   [injector (when (some? value) (fresh-existential module))]))
            (into (empty injectors)))]
      (solve-existential module alpha (assoc solution :injectors injectors*))
      (swap! (:type-checker/facts module) zip/->end)
      (dorun (map
               (fn [alpha-n tau-n]
                 (when (some? alpha-n)
                   (instantiate-to module alpha-n [(apply module tau-n) :kind/type])))
               (vals injectors*)
               (vals injectors))))

    [{:ast/type :existential-variable :id alpha}
     [{:ast/type (:or :function :record)} _]] ; InstBin
    (undefined :instantiate-to/inst-bin.plus)

    [({:ast/type :existential-variable :id alpha}
      :guard #(->> % :id (contains? (existential-restrictions module))))
     _] ; InstSolve⊃
    (let [restrictions (get (existential-restrictions module) alpha)]
      (doseq [restriction restrictions]
        (->> restriction
          (walk/prewalk-replace {type solution})
          (proposition:instance module))
        (solve-existential module alpha solution kind)))

    [{:ast/type :existential-variable :id alpha} _]
    (let [solution (apply module solution)]
      ;; TODO: check for wellformedness
      (solve-existential module alpha solution kind)))) ; InstSolve

(defn- subtyping:equivalent
  "Γ ⊢ A ≡ B ⊣ Δ
  found on pg. 42"
  [module type-a type-b]
  {:pre [(type/is? type-a)
         (type/is? type-b)]}
  (let [type-a (apply module type-a)
        type-b (apply module type-b)]
    (when (not= type-a type-b)
      (match [type-a type-b]
        [{:ast/type :function :domain a-1 :return a-2}
         {:ast/type :function :domain b-1 :return b-2}] ; ≡⊕F
        (do (subtyping:equivalent module a-1 b-1)
            (subtyping:equivalent module (apply module a-2) (apply module b-2)))

        [{:ast/type :variant :injectors injectors-1}
         {:ast/type :variant :injectors injectors-2}] ; ≡⊕V
        (do (when-let [diff (not-empty (utils/symmetric-difference (set (keys injectors-1)) (set (keys injectors-2))))]
              ;; TODO: check that both or neither fields are enums
              (throw
                (ex-info "Different variant types"
                  {:left  type-a
                   :right type-b
                   :diff  diff})))
            (merge-with
              (fn [type-1 type-2]
                (when (and (some? type-1) (some? type-2))
                  (subtyping:equivalent module (apply module type-1) (apply module type-2))))
              injectors-1
              injectors-2)
            nil)

        [{:ast/type :record :fields fields-1}
         {:ast/type :record :fields fields-2}] ; ≡⊕R
        (do (when-let [diff (not-empty (utils/symmetric-difference (set (keys fields-1)) (set (keys fields-2))))]
              (throw
                (ex-info "Different record types"
                  {:left  type-a
                   :right type-b
                   :diff  diff})))
            (merge-with
              (fn [type-1 type-2]
                (subtyping:equivalent module (apply module type-1) (apply module type-2)))
              fields-1
              fields-2)
            nil)

        [{:ast/type :forall :variable alpha-1 :body body-1}
         {:ast/type :forall :variable alpha-2 :body body-2}] ; ≡∀
        (if-not (= alpha-1 alpha-2)
          (throw (ex-info "Different universal variables cannot be equivalent"
                   {:left type-a :right type-b}))
          (let [mark (fresh-mark module)]
            (declare-universal module alpha-1 :kind/type)
            (subtyping:equivalent module body-1 body-2)
            (drop module mark)))

        [{:ast/type :guarded :proposition p :body a}
         {:ast/type :guarded :proposition q :body b}] ; ≡⊃
        (do (proposition:equivalent module p q)
            (subtyping:equivalent module
              (apply module a)
              (apply module b)))

        [{:ast/type :quote :inner inner-1}
         {:ast/type :quote :inner inner-2}]
        (subtyping:equivalent module inner-1 inner-2)

        [{:ast/type :existential-variable} _] ; ≡InstantiateL
        (instantiate-to module type-a [type-b :kind/type])

        [_ {:ast/type :existential-variable}] ; ≡InstantiateR
        (instantiate-to module type-b [type-a :kind/type])

        [{:ast/type :named :name name} _]
        (subtyping:equivalent module (lookup-type module name) type-b)

        [_ {:ast/type :named :name name}]
        (subtyping:equivalent module type-a (lookup-type module name))

        [{:ast/type :application :operator operator-1 :parameters parameters-1}
         {:ast/type :application :operator operator-2 :parameters parameters-2}]
        (do (subtyping:equivalent module operator-1 operator-2)
            (mapv
              (fn [param-1 param-2] (subtyping:equivalent module param-1 param-2))
              parameters-1
              parameters-2)
            nil)

        [{:ast/type :application :operator operator :parameters parameters} _]
        (subtyping:equivalent module
          (-> module
            (lookup-type (:name operator))
            (type/instantiate-universals parameters))
          type-b)

        [_ {:ast/type :application :operator operator :parameters parameters}]
        (subtyping:equivalent module
          type-a
          (-> module
            (lookup-type (:name operator))
            (type/instantiate-universals parameters)))

        [_ _]
        (throw
          (ex-info "Different types"
            {:left  type-a
             :right type-b}))))))

(defn- subtype
  [module polarity type-a type-b]
  {:pre [(polarity? polarity)
         (type/is? type-a)
         (type/is? type-b)]}
  (match [polarity type-a type-b]
    [_ (_ :guard (complement type/quantified?)) (_ :guard (complement type/quantified?))] ; <:Equiv
    (subtyping:equivalent module type-a type-b)

    [:negative {:ast/type :forall :variable universal-alpha :body a} (b :guard (complement type/universally-quantified?))] ; <:∀L
    (let [mark              (fresh-mark module)
          existential-alpha (fresh-existential module)]
      (subtype module :negative
        (walk/prewalk-replace {universal-alpha existential-alpha} a)
        b)
      (drop module mark)
      nil)

    [:negative a {:ast/type :forall :variable universal-beta :body b}] ; <:∀R
    (let [mark (fresh-mark module)]
      (subtype module :negative a b)
      (drop module mark)
      nil)

    [:positive (a :guard type/negative?) (b :guard (complement type/positive?))]
    (subtype module :negative a b)

    [:positive (a :guard (complement type/positive?)) (b :guard type/negative?)]
    (subtype module :negative a b)

    [:negative (a :guard type/positive?) (b :guard (complement type/negative?))]
    (subtype module :positive a b)

    [:negative (a :guard (complement type/negative?)) (b :guard type/positive?)]
    (subtype module :positive a b)

    [_ _ _]
    (throw (ex-info "Invalid subtype" {:polarity polarity :sub type-a :super type-b}))))

(defn- match:check
  "Γ ⊢ Π :: A^→ q ⇐ C p ⊣ Δ
  found on pg. 44"
  [module branches [pattern-types pattern-principality] [return-type return-principality]]
  {:pre [(seq branches)
         (every? :ast/pattern (mapcat :patterns branches))
         (every? (comp term/is? :action) branches)
         (every? type/is? pattern-types) (principality? pattern-principality)
         (type/is? return-type) (principality? return-principality)]}
  (let [return [(apply module return-type) return-principality]]
    (doseq [branch branches] ; MatchSeq + MatchEmpty
      (let [pattern      (first (:patterns branch))
            pattern-type (some->> pattern-types
                           (first)
                           (apply module))]
        (match [pattern
                pattern-type
                pattern-principality]
          [nil nil _]                   ; MatchBase
          (analysis:check module
            (:action branch)
            return)

          [{:ast/pattern :atom :atom atom}
           {:ast/type :primitive :primitive primitive}
           _]                           ; MatchUnit
          (if (= (:atom atom) primitive)
            (match:check module
              [(update branch :patterns next)]
              [(next pattern-types) pattern-principality]
              return)
            (throw (ex-info "Distinct types" {:atom atom :primitive primitive})))

          [{:ast/pattern :atom :atom atom}
           {:ast/type :existential-variable :id alpha}
           :non-principal]              ; MatchUnitα^
          (let [type (synthesize-atom atom)]
            (solve-existential module alpha type)
            (match:check module
              [branch]
              [(cons type (next pattern-types)) :principal]
              return))

          [{:ast/pattern :variant :variant {:injector injector :value value}}
           {:ast/type :variant :injectors injectors}
           _]                           ; Match+ₖ
          (cond
            (and (some? value) (some? (get injectors injector)))
            (match:check module
              [(update branch :patterns (fn [patterns] (cons value (next patterns))))]
              [(cons (get injectors injector) (next pattern-types)) pattern-principality]
              return)

            (and (nil? value) (contains? injectors injector))
            (match:check module
              [(update branch :patterns next)]
              [(next pattern-types) pattern-principality]
              return)

            :default
            (throw (ex-info "Unknown injector" {:injector injector})))

          [{:ast/pattern :variant :variant {:injector injector}}
           {:ast/type :existential-variable :id alpha} ; Match+ₖα^
           :non-principal]
          (let [current (zip/node @(:type-checker/facts module))
                _       (swap! (:type-checker/facts module)
                          zip/focus-left
                          {:fact/declare-existential alpha
                           :kind                     :kind/type})
                type
                (find-variant module injector)]
            (swap! (:type-checker/facts module) zip/focus-right current)
            (solve-existential module alpha type)
            (match:check module
              branches
              [(cons type (next pattern-types)) pattern-principality]
              return))

          [{:ast/pattern :record :fields pattern-fields}
           {:ast/type :record :fields type-fields}
           _]                           ; Match×
          (cond
            (set/subset? (set (keys pattern-fields)) (set (keys type-fields)))
            (match:check module
              [(update branch :patterns (fn [patterns] (concat (vals pattern-fields) (next patterns))))]
              [(concat
                 (map (fn [field] (get type-fields field)) (keys pattern-fields))
                 (next pattern-types))
               pattern-principality]
              return)

            :default
            (throw (ex-info "Unknown record"
                     {:fields (keys pattern-fields)
                      :module (:name module)})))

          [{:ast/pattern :symbol :symbol symbol}
           pattern-type
           _]                           ; MatchNeg
          (let [mark (fresh-mark module)]
            (bind-symbol module symbol pattern-type :principal)
            (match:check module
              [(update branch :patterns next)]
              [(next pattern-types) pattern-principality]
              return)) 

          [{:ast/pattern :wildcard}
           _ _]                         ; MatchWild
          (match:check module
            [(update branch :patterns next)]
            [(next pattern-types) pattern-principality]
            return)

          [_
           {:ast/type :application :operator operator :parameters parameters}
           _] 
          (let [type (-> module
                       (lookup-type (:name operator))
                       (type/instantiate-universals parameters))]
            (match:check module
              branches
              [(cons type (next pattern-types)) pattern-principality]
              return))

          [_
           {:ast/type :named :name name}
           _]
          (let [type (lookup-type module name)]
            (match:check module
              branches
              [(cons type (next pattern-types)) pattern-principality]
              return)))
        (annotate-pattern pattern pattern-type)))))

(defn- apply-spine
  "Γ ⊢ s : A p ≫ C q ⊣ Δ
  found on pg. 37"
  [module arguments [type principality]]
  {:pre [(every? term/is? arguments)
         (type/is? type) (principality? principality)]}
  (match [arguments type principality]
    [[] _ _]                          ; EmptySpine
    [type principality]

    [_ {:ast/type :forall :variable universal-variable :body body} _] ; ∀Spine
    (let [existential-variable (fresh-existential module)
          body (walk/prewalk-replace {universal-variable existential-variable} body)]
      (equate-universal module (:id universal-variable) existential-variable)
      (apply-spine module arguments [body principality]))

    [_ {:ast/type :guarded :proposition proposition :body body} _] ; ⊃Spine
    (let [result (apply-spine module
                   arguments
                   [(apply module body) principality])]
      (proposition:true module proposition)
      result)

    [[e & s] {:ast/type :function :domain domain :return return} _] ; →Spine
    (do (analysis:check module e [domain principality])
        (apply-spine module
          s
          [(apply module return) principality]))

    [_ {:ast/type :existential-variable :id alpha} :non-principal] ; α^Spine
    (let [current  (zip/node @(:type-checker/facts module))
          _        (swap! (:type-checker/facts module)
                     zip/focus-left
                     {:fact/declare-existential alpha
                      :kind                     :kind/type})
          alpha-1  (fresh-existential module)
          alpha-2  (fresh-existential module)
          function {:ast/type :function
                    :domain   alpha-1
                    :return   alpha-2}]
      (solve-existential module alpha function :kind/type)
      (swap! (:type-checker/facts module) zip/focus-right current)
      (apply-spine module
        arguments
        [function principality]))))

(defn- recovering-apply-spine
  "Γ ⊢ s : A p ≫ C ⌈q⌉ ⊣ Δ
  found on pg. 37"
  [module arguments [type principality]]
  (let [[type principality*]
        (apply-spine module arguments [type principality])
        type (apply module type)]
    (cond
      (and ; SpineRecover
        (= principality* :non-principal)
        (empty? (type/free-existential-variables type)))
      [type :principal] 

      (or ; SpinePass
        (= principality :non-principal)
        (= principality* :principal)
        (not-empty (type/free-existential-variables type)))
      [type principality*]

      :else (undefined ::recovering-apply-spine))))

(defn to-jvm-type
  [module type]
  (match type
    {:ast/type :named :name name}
    (->> name
      (lookup-type module)
      (recur module))

    {:ast/type :primitive}
    (get jvm/primitives type)

    {:ast/type :object :class class}
    class))

(defn- from-jvm-type
  [type]
  {:ast/type :named
   :name     {:reference :type
              :in        {:reference :module :name ["lang" "builtin"]}
              :name      (match type
                           'void                 "Unit"
                           'int                  "int"
                           'java.lang.String     "String"
                           'java.math.BigInteger "Integer"
                           'java.lang.Boolean    "Bool"
                           'java.lang.Object     "Object")}})

(defn expand-macro
  [module term]
  (match term
    {:ast/term :application :function {:symbol symbol}}
    (let [macro (lookup-macro module symbol)
          term* (merge
                  ((:expand macro) term)
                  (select-keys term [:type-checker.term/type]))]
      (deliver (:type-checker.macro/expands-to term) term*)
      term*)))

(defn synthesize
  "Γ ⊢ e ⇒ A p ⊣ Δ
  found on pg. 37"
  [module term]
  (let [mark (fresh-mark module)
        [type principality]
        (match term
          {:ast/term :symbol :symbol (symbol :guard jvm/native?)}
          (let [class (->> symbol :in :name
                        (str/join ".")
                        (java.lang.Class/forName)
                        (clojure.reflect/reflect))
                field (first (filter #(= (:name symbol) (name (:name %))) (:members class)))]
            [{:ast/type :object :class (:type field)}
             :principal])

          {:ast/term :symbol :symbol symbol}
          (lookup-binding module symbol)

          {:ast/term :application
           :function {:symbol (symbol :guard (partial module/macro? module))}}
          (->> term
            (expand-macro module)
            (synthesize module))

          {:ast/term :application :function function :arguments arguments}
          (->> function
            (synthesize module)
            (recovering-apply-spine module arguments))

          {:ast/term :access
           :object   object
           :field    {:ast/term  :application
                      :function  function
                      :arguments arguments}} ; instance method
          (let [object-type           (->> object
                                        (synthesize module)
                                        (first)
                                        (to-jvm-type module))
                object-bases (->> object-type
                               (name)
                               (Class/forName)
                               (bases)
                               (map (fn [class] (symbol (. class Class/getName))))
                               (set))
                parameter-types       (mapv
                                        #(->> %
                                           (synthesize module)
                                           (first)
                                           (to-jvm-type module))
                                        arguments)
                {:keys [return-type]} (->> function :symbol :in :name
                                        (str/join ".")
                                        (java.lang.Class/forName)
                                        (clojure.reflect/reflect)
                                        :members
                                        (filter (fn [member]
                                                  (and
                                                    (contains? (conj object-bases object-type) (:declaring-class member))
                                                    (contains? (:flags member) :public)
                                                    (= (name (:name member)) (:name (:symbol function)))
                                                    (= (:parameter-types member) parameter-types))))
                                        (first))]
            (annotate-term function
              {:ast/type  :method
               :class     object-type
               :signature (conj parameter-types return-type)})
            [(from-jvm-type return-type) :principal])

          {:ast/term :access
           :object   object} ; static field
          (undefined ::static-field)

          {:ast/term :quote
           :body     body}
          (let [alpha (fresh-existential module)]
            (analysis:check module body [alpha :non-principal])
            [{:ast/type :quote :inner (get (existential-solutions module) alpha alpha)} :non-principal])

          {:ast/term :unquote
           :body body}
          (let [[type principality] (synthesize module body)]
            (match type
              {:ast/type :quote :inner inner}
              [inner principality]))

          {:ast/term _}
          (let [alpha (fresh-existential module)]
            (analysis:check module term [alpha :non-principal])
            [(get (existential-solutions module) alpha alpha) :non-principal]))
        type (->> type
                (generalize module mark)
                (apply module))]
    (annotate-term term type)
    [type principality]))

(defn- abstract-type
  [module {:keys [name params body]}]
  (letfn [(universally-quantify-parameters [params type]
            (let [param-type->universal-variable
                  (->> params
                    (mapv (fn [param]
                            (-> module
                              (fresh-universal)
                              (assoc :reference param))))
                    (zipmap (map (fn [param] {:ast/type :named :name param}) params)))]
              (->> params
                (reverse)
                (reduce
                  (fn [type param]
                    {:ast/type :forall
                     :variable (get param-type->universal-variable {:ast/type :named :name param})
                     :body     type})
                  (walk/prewalk-replace param-type->universal-variable type)))))]
    (->> body
      (universally-quantify-parameters params)
      (apply module))))

(defn- abstract-typeclass
  [module {:keys [name params fields]}]
  (letfn [(universally-quantify-parameters [params type]
            (let [param-type->universal-variable
                  (->> params
                    (mapv (fn [param]
                            (-> module
                              (fresh-universal)
                              (assoc :reference param))))
                    (zipmap (map (fn [param] {:ast/type :named :name param}) params)))]
              (->> params
                (reverse)
                (reduce
                  (fn [type param]
                    {:ast/type :forall
                     :variable (get param-type->universal-variable {:ast/type :named :name param})
                     :body     type})
                  (walk/prewalk-replace param-type->universal-variable type)))))
          (universally-quantify-unknowns [type]
            (let [named-types   (->> type
                                  (type/nodes)
                                  (keep #(when (-> % :ast/type (= :named))
                                           (:name %)))
                                  (set))
                  known-types   (set/union
                                  (set (keys (module/all-types module)))
                                  (set params))
                  unknown-types (set/difference named-types known-types)]
              (->> unknown-types
                (reverse)
                (reduce
                  (fn [inner type]
                    (let [universal (assoc (fresh-universal module) :reference type)]
                      {:ast/type :forall
                       :variable universal
                       :body     (walk/prewalk-replace {{:ast/type :named :name type} universal} inner)}))
                  type))))]
    (let [fields (->> fields
                   (map (fn [[k v]]
                          [k
                           (->>
                             {:ast/type :guarded
                              :proposition
                              {:ast/constraint :instance
                               :typeclass      name
                               :parameters     (mapv (fn [param] {:ast/type :named :name param}) params)}
                              :body     (universally-quantify-unknowns v)}
                             (universally-quantify-parameters params)
                             (apply module))]))
                   (into (empty fields)))]
      {:name       name
       :parameters params
       :fields     fields})))

(defn- instantiate-typeclass
  "Check an instance declaration against the typeclass.

  Checks the instance declaration against the type resulting from
  substituting the specified parameters into the typeclass
  declaration.

  Takes care of detecting universal parameters and enforcing
  superclass constraints."
  [module {:keys [name superclasses types fields] :as instance}]
  (letfn [(strip-guard [type]
            (match type
              {:ast/type :guarded :body body}
              body

              _ (throw (ex-info "Unguarded type" {:type type}))))
          (abstract-parameters [types] 
            ;; detect typeclass parameters that aren't real types, such as T in (Show (Option T)) and replace them with universal variables
            (let [substitutions
                  (reduce
                    (fn [type->universal-variable type]
                      (->> type
                        (type/nodes)
                        (keep (fn [type]
                                (match type
                                  {:ast/type :named}
                                  (when (and
                                          (not (:in (:name type)))
                                          (not (contains? type->universal-variable type)))
                                    [type (assoc (fresh-universal module) :reference type)])

                                  _ nil)))
                        (into {})
                        (merge type->universal-variable)))
                    nil
                    types)]
              [(walk/postwalk-replace substitutions types) (not-empty substitutions)]))
          (constrain-superclasses [type superclasses]
            ;; introduces typeclass constraints for typeclass field types for any superclasses of the instance
            (reduce
              (fn [type superclass]
                {:ast/type :guarded
                 :proposition superclass
                 :body type})
              type
              superclasses))
          (introduce-universal-parameters [type universal-variables]
            ;; introduces `:forall`s for any universal parameters, as detected by `abstract-parameters`
            (reduce
              (fn [type universal]
                {:ast/type :forall
                 :variable universal
                 :body type})
              type
              universal-variables))]
    (if-let [typeclass (-> module
                         (module/all-typeclasses)
                         (module/get name))]
      (let [[types variables] (->> types
                                (mapv (partial apply module))
                                (abstract-parameters))
            superclasses      (->> superclasses
                                (map
                                  (fn [[typeclass parameters]]
                                    {:ast/constraint :instance
                                     :typeclass      typeclass
                                     :parameters     parameters}))
                                (into #{})
                                (walk/postwalk-replace variables)
                                (not-empty))
            instantiation     {:types        types
                               :variables    variables
                               :superclasses superclasses}
            module            (update module :typeclasses
                                (partial clojure.core/apply utils/deep-merge)
                                {name {:instances #{instantiation}}}
                                (map
                                  (fn [{:keys [typeclass parameters]}]
                                    {typeclass {:instances #{{:types parameters}}}})
                                  superclasses))]
        (run!
          (fn [[field term]]
            (let [type 
                  (-> typeclass
                    (get-in [:fields field])
                    (type/instantiate-universals types)
                    (strip-guard)
                    (constrain-superclasses superclasses)
                    (introduce-universal-parameters (vals variables)))]
              (analysis:check module term [type :principal])))
          fields)
        instantiation)
      (throw (ex-info "Unknown typeclass" {:typeclass name})))))

(defn- init
  [module]
  {:pre [(ast/module? module)]}
  (merge module
    {:type-checker/current-variable (atom 0)
     :type-checker/facts            (atom zip/empty)
     :type-checker/instance-chains  (atom {})
     :definitions                   []}))

(defn run
  [module]
  {:pre [(ast/module? module)]}
  (->> module
    :definitions
    (reduce
      (fn [module definition]
        (let [definition (setup-annotations definition)
              module
              (match definition
                {:ast/definition :type
                 :name           name}
                (let [type*       (abstract-type module definition)
                      definition* (assoc definition :body type*)]
                  (-> module
                    (assoc-in [:types name] type*)
                    (update :definitions conj definition*)))

                {:ast/definition :typeclass
                 :name           name
                 :params         params
                 :fields         fields}
                (-> module
                  (assoc-in [:typeclasses name] (abstract-typeclass module definition))
                  (update :definitions conj definition))

                {:ast/definition :typeclass-instance
                 :name           typeclass
                 :types          types
                 :fields         fields}
                (let [instantiation (instantiate-typeclass module definition)
                      definition    (resolve-annotations module (merge definition instantiation))]
                  (-> module
                    (update-in [:typeclasses typeclass :instances] (fnil conj #{}) instantiation)
                    (update :definitions conj definition)))

                {:ast/definition :constant
                 :name           name
                 :body           expr}
                (let [[type principality] (synthesize module expr)
                      definition          (assoc definition
                                            :body (resolve-annotations module expr)
                                            :type-checker.term/type type)]
                  (-> module
                    (assoc-in [:values name] [type :principal])
                    (update :definitions conj definition)))

                {:ast/definition :macro
                 :name           name
                 :arguments      arguments
                 :body           body}
                (let [mark (fresh-mark module)
                      argument-types
                      (mapv (fn [argument]
                              (let [alpha (fresh-existential module :kind/type)
                                    type  {:ast/type :quote :inner alpha}]
                                (bind-symbol module argument type :non-principal)
                                type))
                        arguments)
                      return-type (fresh-existential module)
                      _           (analysis:check module body [return-type :non-principal])
                      type        [(->> return-type
                                     (conj argument-types)
                                     (type/function)
                                     (generalize module mark))
                                   :non-principal]]
                  (-> module
                    (assoc-in [:macros name]
                      {:type      type
                       :arguments arguments
                       :expand    (fn [application]
                                    (let [environment (zipmap arguments (:arguments application))]
                                      (interpreter/run environment body)))})
                    (update :definitions conj definition))))]
          (swap! (:type-checker/instance-chains module)
                    (fn [chains]
                      (walk/prewalk
                        #(if (-> % :ast/type (= :existential-variable))
                           (apply module %)
                           %)
                        chains)))
          (reset! (:type-checker/facts module) zip/empty)
          module))
      (init module))))


(comment

  (letfn [(run [path]
            (-> path
              (lang.compiler/run :until :type-checker)
              (module/signature)
              (println)))]
    (do (println "\n–-—")
        (run "std/lang/option.lang")
        (run "std/lang/list.lang")
        (run "std/lang/math.lang")
        (run "std/lang/io.lang")
        (run "std/lang/core.lang")
        (run "examples/option.lang")
        (run "examples/linked-list.lang")
        (run "examples/arithmetic.lang")))

  (throw (ex-info "foo" {}))

  (com.gfredericks.debug-repl/unbreak!!)

  (com.gfredericks.debug-repl/unbreak!)

  (-> "examples/linked-list.lang"
    (lang.compiler/run :until :type-checker)
    :values)

  (do (println "\n–-—")
      (-> "std/lang/option.lang"
        (lang.compiler/run :until :type-checker)
        (module/surface-bindings)))

  )
