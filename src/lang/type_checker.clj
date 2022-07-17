(ns lang.type-checker
  (:refer-clojure :exclude [apply drop type])
  (:require [clojure.core.match :refer [match]]
            clojure.reflect
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [lang.ast :as ast]
            [lang.db :as db]
            [lang.interpreter :as interpreter]
            [lang.jvm :as jvm]
            [lang.module :as module]
            [lang.term :as term]
            [lang.type :as type]
            [lang.utils :as utils :refer [undefined]]
            [lang.zip :as zip]
            [taoensso.timbre :as log]
            [lang.definition :as definition])
  (:import java.lang.Class))

(def ^:dynamic *context* nil)

(defn surface-types
  [module]
  (->> module
       :definitions
       (keep (fn [{kind :ast/definition
                   :as definition}]
               (when (= :type kind)
                 definition)))
       (map (fn [{:keys [db/id name]}]
              [(select-keys name [:ast/reference :name]) (db/->ref id)]))
       (into {})))

(defn imported-types
  [module]
  (let [db @db/state]
    (->> module
         :imports
         (mapcat
          (fn [{:keys [module alias open]}]
            (->> module
                 (db/->entity db)
                 surface-types
                 (map (fn [[n r]]
                        [(cond
                           alias (assoc n :in alias)
                           open n)
                         r])))))
         (into {}))))

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
  []
  (swap! (:type-checker/current-variable *context*) inc))

(defn- fresh-existential
  ([]
   (fresh-existential :kind/type))
  ([kind]
   (let [variable-id (next-variable-id)
         type    {:ast/type :existential-variable
                  :id       variable-id}]
     (swap! (:type-checker/facts *context*)
       zip/insert-left
       {:fact/declare-existential variable-id
        :kind                     kind})
     type)))

(defn- fresh-universal
  ([]
   (fresh-universal :kind/type))
  ([kind]
   (let [variable-id (next-variable-id)
         type        {:ast/type :universal-variable
                      :id       variable-id}]
     (swap! (:type-checker/facts *context*)
       zip/insert-left
       {:fact/declare-universal variable-id
        :kind                   kind})
     type)))

(defn- declare-universal
  [{:keys [id] :as universal} kind]
  {:pre [(associative? universal) (some? id)]}
  (swap! (:type-checker/facts *context*)
    zip/insert-left
    {:fact/declare-universal id
     :kind                   kind})
  nil)

(defn- fresh-mark
  []
  (let [marker {:fact/marker (next-variable-id)}]
    (swap! (:type-checker/facts *context*) zip/insert-left marker)
    marker))

(defn- drop
  [fact]
  (let [zipper
        (swap! (:type-checker/facts *context*)
          #(-> %
             (zip/->end)
             (zip/focus-left fact)
             (zip/left)))
        [remainder discard] (zip/split zipper)]
    (reset! (:type-checker/facts *context*) remainder)
    discard))

(defn- bind-symbol
  [{:db/keys [id]} type principality]
  (swap! (:type-checker/facts *context*)
    zip/insert-left
    {:fact/bind-symbol (db/->ref id)
     :type             type
     :principality     principality})
  nil)

(defn- existential-facts
  []
  (->> *context*
    :type-checker/facts
    (deref)
    (zip/left-seq)
    (keep (fn [fact]
            (some-> fact
              ((some-fn :fact/solve-existential :fact/restrict-existential :fact/declare-existential))
              (vector fact))))
    (into {})))

(defn- existential-solutions
  []
  (->>
    (existential-facts)
    (keep (fn [[k v]]
            (match v
              {:fact/solve-existential _}
              [k (:type v)]

              _ nil)))
    (into {})))

(defn- existential-restrictions
  []
  (->>
    (existential-facts)
    (keep (fn [[k v]]
            (match v
              {:fact/restrict-existential _ :constraints constraints}
              {k constraints}

              _ nil)))
    (reduce (partial merge-with set/union))))

(defn- universal-facts
  []
  (->> *context*
    :type-checker/facts
    (deref)
    (zip/left-seq)
    (keep (fn [fact]
            (some-> fact
              ((some-fn :fact/declare-universal :fact/constrain-universal :fact/equate-universal))
              (vector fact))))))

(defn- universal-equations
  []
  (->>
    (universal-facts)
    (keep (fn [[k v]]
            (match v
              {:fact/equate-universal _}
              [k (:type v)]

              _ nil)))
    (into {})))

(defn- universal-constraints
  []
  (->>
    (universal-facts)
    (keep (fn [[k v]]
            (match v
              {:fact/constrain-universal _}
              {k (:constraints v)}

              _ nil)))
    (reduce (partial merge-with set/union))))

(defn- unsolved?
  [existential-variable]
  (not
    (or
      (contains? (existential-solutions) existential-variable)
      (contains? (existential-restrictions) existential-variable))))

(defn- restricted?
  [existential-variable]
  (contains? (existential-restrictions) existential-variable))

(defn- equated?
  [universal-variable]
  (contains? (universal-equations) universal-variable))

(defn- constrained?
  [universal-variable]
  (contains? (universal-constraints) universal-variable))

(defn- local-bindings
  []
  (->> *context*
    :type-checker/facts
    (deref)
    (zip/left-seq)
    (keep (fn [fact]
            (when-let [symbol (:fact/bind-symbol fact)]
              [symbol ((juxt :type :principality) fact)])))
    (into {})))

(defn- lookup-binding
  [module ref]
  ;; {:pre [(db/ref? symbol)]}
  (or
   (and (db/ref? ref) (get (local-bindings) ref)) ; local ref
   (and (db/ref? ref) (:type (db/->entity @db/state ref))) ; global ref
   (undefined ::lookup-binding)
   (throw (ex-info "Unknown binding"
                   {:symbol ref
                    :module (:name module)}))))

(defn- lookup-type
  [module ref]
  (try
    (or
     (:body (db/->entity @db/state ref))
     (throw (ex-info "Unknown type"
                     {:type     name
                      :module   (:name module)})))
    (catch Throwable t
      (undefined ::lookup-type))))

(comment
  ref
  t

  )

(defn- lookup-macro
  [module ref]
  (db/touch (db/->entity @db/state ref)))

(defn- apply
  [type]
  (match type
    {:ast/type :existential-variable :id id}
    (or
      (some->> id
        (get (existential-solutions))
        apply)
      type)

    {:ast/type :universal-variable :id id}
    (or
      (some->> id
        (get (universal-equations))
        apply)
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
        (update :variable apply)
        (update :body apply)
        (unwrap-nonuniversal)))

    {:ast/type :guarded}
    (-> type
      (update-in [:proposition :parameters] (partial mapv apply))
      (update :body apply))

    {:ast/type :application}
    (letfn [(collect-nested-applications [type]
              (match type
                     {:ast/type :application :operator {:ast/type :application :operator operator :parameters inner-params} :parameters outer-params}
                     {:ast/type :application
                      :operator operator
                      :parameters (into inner-params outer-params)}

                     _ type))]
      (-> type
          (update :operator apply)
          (update :parameters (partial mapv apply))
          (collect-nested-applications)))

    {:ast/type :function}
    (-> type
      (update :domain apply)
      (update :return apply))

    {:ast/type :variant}
    (update type :injectors
      #(->> %
         (map (fn [[injector value]] [injector (some->> value apply)]))
         (into (empty %))))

    {:ast/type :record}
    (update type :fields
      #(->> %
         (map (fn [[field content]] [field (apply content)]))
         (into (empty %))))

    {:ast/type :primitive}
    type

    {:ast/type :named :name name}
    type

    {:ast/type (:or :object :method)}
    type

    {:ast/type :quote}
    (update type :inner apply)

    _
    (undefined :apply/fallthrough)))

(defn- solve-existential
  ([existential solution]
   (solve-existential existential solution :kind/type))
  ([existential solution kind]
   {:pre [(int? existential) (associative? solution)]}
   (let [current-fact (get (existential-facts) existential)
         new-fact
         {:fact/solve-existential existential
          :kind                   kind
          :type                   (apply solution)}]
     (swap! (:type-checker/facts *context*)
       #(walk/prewalk-replace           ; FIXME
          {current-fact new-fact}
          %))
     nil)))

(defn- restrict-existential
  ([existential constraints]
   (restrict-existential existential constraints :kind/type))
  ([existential constraints kind]
   {:pre [(int? existential) (set? constraints)]}
   (let [current-fact (get (existential-facts) existential)
         restrictions (get (existential-restrictions) existential)
         new-fact
         {:fact/restrict-existential existential
          :kind                      kind
          :constraints               (set/union restrictions constraints)}]
     (swap! (:type-checker/facts *context*)
       #(walk/prewalk-replace
          {current-fact new-fact}
          %))
     nil)))

(defn- equate-universal
  [universal solution]
  {:pre [(int? universal) (type/is? solution)]}
  (swap! (:type-checker/facts *context*)
    zip/insert-left
    {:fact/equate-universal universal
     :type solution})
  nil)

(defn- constrain-universal
  [universal constraints]
  {:pre [(int? universal) (set? constraints)]}
  ;; TODO: assert no equation
  (swap! (:type-checker/facts *context*)
    zip/insert-left
    {:fact/constrain-universal universal
     :constraints constraints})
  nil)

(defn- clear-existential
  [existential]
  {:pre [(int? existential)]}
  (swap! (:type-checker/facts *context*)
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
  (let [{parameters :params name :name}
        (:belongs-to (db/->entity @db/state injector))
        type {:ast/type :named :name name}]
    (if (seq parameters)
      {:ast/type   :application
       :operator   type
       :parameters (mapv (fn [_] (fresh-existential)) parameters)}
      type)))

(defn- find-record
  [module fields]
  (let [types      (map
                (partial get (module/all-fields module))
                (keys fields))
        _          (assert (clojure.core/apply = types))
        type       (first types)
        parameters (->> type
                     :name
                     (lookup-type module)
                     (type/parameters)
                     (mapv (fn [_] (fresh-existential))))]
    (if (seq parameters)
      {:ast/type   :application
       :operator   type
       :parameters parameters}
      type)))

(defn- annotate-term
  [term type]
  (when-let [promise (:type-checker.term/type term)]
    (deliver promise type)))

(defn- annotate-pattern
  [pattern type]
  (when-let [promise (:type-checker.pattern/type pattern)]
    (deliver promise type)))

(defn- generalize
  [mark type]
  (let [type    (apply type)
        discard (drop mark)
        universal-variables
        (reduce
          (fn [universal-variables
               {:fact/keys [declare-existential restrict-existential]
                :keys      [kind constraints]
                :as        fact}]
            (let [existential-variable-id (or declare-existential restrict-existential)
                  existential-variable    {:ast/type :existential-variable
                                           :id       existential-variable-id}]
              (if (some? existential-variable-id)
                (let [universal-variable (fresh-universal)
                      constraints        (walk/prewalk-replace
                                           {existential-variable universal-variable}
                                           constraints)]
                  (swap! (:type-checker/facts *context*)
                    #(-> %
                       (zip/insert-right {:fact/solve-existential existential-variable-id
                                          :kind                   kind
                                          :type                   universal-variable})
                       (zip/right)))
                  (conj universal-variables
                    {:existential  existential-variable
                     :universal    universal-variable
                     :restrictions constraints}))
                (do (swap! (:type-checker/facts *context*)
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
            (fn [type {:keys [existential universal restrictions]}]
              (let [restrictions
                    (->> restrictions
                      (map #(update % :parameters (partial mapv apply)))
                      (into (empty restrictions)))]
                ;; (undefined ::generalize)

                [type universal-variables
                 existential
                 (some (partial = existential) (type/nodes type))
                 (type/contains? type existential)
                 ]

                (cond
                  (and (type/contains? type existential)
                       (not-empty restrictions))
                  (let [proposition (if (= 1 (count restrictions)) ; FIXME: multiple restrictions
                                      (first restrictions)
                                      (undefined ::multiple-constraints))]
                    {:ast/type :forall
                     :variable universal
                     :body     {:ast/type    :guarded
                                :proposition proposition
                                :body        type}})

                  (type/contains? type existential)
                  {:ast/type :forall
                   :variable universal
                   :body     type}

                  :else type)))
            type)
          apply)]
    generalized-type))

(defn- synthesize-atom
  [atom]
  (or
   (let [builtins (db/->entity @db/state
                               [:name {:ast/reference :module
                                       :name ["lang" "native" "jvm"]}])]
     (get (->> builtins
               :definitions
               (keep (fn [{:keys [ast/definition db/id body]}]
                       (when (and (= definition :type) (:primitive body))
                         [(:primitive body) {:ast/type :named :name (db/->ref id)}])))
               (into {}))
          (:atom atom)))
   (undefined ::synthesize-atom)
   (throw (ex-info "Unrecognized atom" {:atom atom}))))

(defn- incompatible-head-constructors?
  [type-1 type-2]
  (not= (:ast/type type-1) (:ast/type type-2)))

(defn- unify
  [type-a type-b kind]
  (when-not (and
              (contains? #{:universal-variable :primitive :named} (:ast/type type-a))
              (= type-a type-b)) ; ElimeqUvarRefl ElimeqUnit
    (match [type-a type-b]
      [{:ast/type :universal-variable :id alpha} _]
      (cond
        (and
          (not (contains? (type/free-variables type-b) type-a))
          (constrained? alpha))
        (undefined ::unify.uvar-l.constrained)

        (and
          (not (contains? (type/free-variables type-b) type-a))
          (not (equated? alpha))) ; ElimeqUvarL
        (equate-universal alpha type-b)

        (and
          (not= type-a type-b)
          (contains? (type/free-variables type-b) type-a)) ; ElimeqUvarL⊥
        (bottom))

      [_ {:ast/type :universal-variable :id alpha}]
      (cond
        (and
          (not (contains? (type/free-variables type-a) type-b))
          (constrained? alpha))
        (undefined ::unify.uvar-r.constrained)

        (and
          (not (contains? (type/free-variables type-a) type-b))
          (not (equated? alpha))) ; ElimeqUvarR
        (equate-universal alpha type-a)

        (and
          (not= type-b type-a)
          (contains? (type/free-variables type-a) type-b)) ; ElimeqUvarR⊥
        (bottom))

      [{:ast/type :function :domain domain-1 :return return-1}
       {:ast/type :function :domain domain-2 :return return-2}] ; ElimeqBinF
      (do (unify domain-1 domain-2 kind)
          (unify (apply return-1) (apply return-2) kind))

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
      (do (unify operator-1 operator-2 kind)
          (->>
            (map vector parameters-1 parameters-2)
            (run! (fn [[param-1 param-2]] (unify param-1 param-2 kind)))))

      [_ _] ; ElimeqClash
      (if (incompatible-head-constructors? type-a type-b)
        (bottom)
        (undefined ::unify.fallthrough)))))

(defn- proposition:instance
  [module proposition]
  (letfn [(init [{:keys [variables] :as instance}]
            (run!
              #(declare-universal % :kind/type)
              (vals variables))
            instance)
          (match-head [{:keys [parameters] :as proposition}
                       {:keys [superclasses types]}]
            (when
                (->>
                  (map vector types parameters)
                  (every? (fn [[type parameter]]
                            (unify type (apply parameter) :kind/type)
                            true)))
              [(assoc proposition :parameters types) superclasses]))
          (match-superclasses [[instance superclasses]]
            (->> superclasses
              (mapcat (partial proposition:instance module))
              (cons instance)
              (doall)))
          (find-chain [proposition instance]
            (let [mark (fresh-mark)]
              (try
                (->> instance
                  (init)
                  (match-head proposition)
                  (match-superclasses))
                (catch Exception e
                  (drop mark)
                  nil))))]
    (let [proposition (update proposition :parameters #(mapv apply %))]
      (match proposition
        {:ast/constraint :instance :typeclass typeclass :parameters parameters}
        (let [{:keys [instances] :as typeclass}
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
                        (restrict-existential alpha #{proposition})

                        {:ast/type :universal-variable :id alpha}
                        (constrain-universal alpha #{proposition}))
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
            (swap! (:type-checker/instance-chains *context*)
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

(defn- denominalize
  [module type]
  (match type
    {:ast/type :named :name name}
    (lookup-type module name)

    {:ast/type   :application
     :operator   operator
     :parameters parameters}
    (type/instantiate-universals (denominalize module operator) parameters)))

(defn- analysis:check
  "Γ ⊢ e ⇐ A p ⊣ Δ
  found on pg. 37"
  [module term [type principality]]
  {:pre [(ast/module? module)
         (term/is? term)
         (type/is? type) principality? principality?]}
  (let [type (apply type)]
    (annotate-term term type)
    (match [term type principality]
      [{:ast/term :recur :reference reference :body body}
       _ _] ; Rec
      (let [marker (fresh-mark)]
        (bind-symbol reference type principality)
        (analysis:check module body [type principality])
        ;; FIXME: paper says to, but we don't drop here, otherwise the resolved type annotations on typeclass member functions will contain original universals
        #_(drop marker))

      [{:ast/term :atom :atom atom}
       {:ast/type :existential-variable :id alpha}
       :non-principal] ; 1Iα^
      (let [type (synthesize-atom atom)]
        (solve-existential alpha type))

      [{:ast/term :atom :atom atom}
       {:ast/type :primitive :primitive primitive}
       _] ; 1I
      (when-not (= (:atom atom) primitive)
        (throw (ex-info "Distinct types" {:atom atom :primitive primitive})))

      [(_ :guard checked-intro-form?)
       {:ast/type :forall :variable alpha :body body}
       _] ; ∀I
      (do (declare-universal alpha :kind/type)
          (analysis:check module term [body principality]))

      [(_ :guard checked-intro-form?)
       {:ast/type :guarded :proposition proposition :body body}
       :principal] ; ⊃I / ⊃I⊥
      (let [mark (fresh-mark)]
        (proposition:incorporate module proposition)
        (analysis:check module term [(apply body) :principal])
        nil)

      [{:ast/term :lambda :arguments arguments :body body}
       {:ast/type :function}
       _] ; →I
      (let [return
            (reduce
              (fn [type argument]
                (match type
                  {:ast/type :function :domain domain :return return}
                  (do (bind-symbol argument domain principality)
                      return)))
              type
              arguments)]
          (analysis:check module body [return principality]))

      [{:ast/term :lambda :arguments arguments :body body}
       ({:ast/type :existential-variable :id alpha} :as alpha-type)
       :non-principal] ; →Iα^
      (let [current         (zip/node @(:type-checker/facts *context*))
            _               (swap! (:type-checker/facts *context*)
                              zip/focus-left
                              {:fact/declare-existential alpha :kind :kind/type})
            mark (fresh-mark)
            argument-alphas (mapv
                              (fn [arg]
                                (or (some->> arg :type apply)
                                    (fresh-existential)))
                              arguments)
            return-alpha    (fresh-existential)
            function        (reduce
                              (fn [return argument]
                                {:ast/type :function
                                 :domain   argument
                                 :return   return})
                              return-alpha
                              argument-alphas)]
        (solve-existential alpha function) ; rm?
        (swap! (:type-checker/facts *context*) zip/focus-right current)
        (analysis:check module term [function :non-principal])
        (solve-existential alpha (generalize mark function)))

      [{:ast/term :match :body body :branches branches} _ _] ; Case
      (let [[pattern-type pattern-principality] (synthesize module body)]
        (match:check module
          branches
          [[pattern-type] pattern-principality]
          [type principality])
        nil)

      [{:ast/term :variant :injector injector :value value}
       {:ast/type :variant :injectors injectors}
       _] ; +I
      (cond
        (and (some? value) (some? (get injectors injector))) ; variant wraps value
        (analysis:check module value [(get injectors injector) principality])

        (and (nil? value) (contains? injectors injector)) ; variant is enum
        nil

        :default ; injector isn't defined
        (throw (ex-info "Unknown injector" {:injector injector})))

      [{:ast/term :variant :injector injector}
       ({:ast/type :existential-variable :id alpha} :as existential-type)
       _] ; +Iα^
      (let [current (zip/node @(:type-checker/facts *context*))
            _       (swap! (:type-checker/facts *context*)
                      zip/focus-left
                      (get (existential-facts) alpha))
            type
            (find-variant module injector)]
        (swap! (:type-checker/facts *context*) zip/focus-right current)
        (instantiate-to module existential-type [type :kind/type])
        (analysis:check module term [(denominalize module type) principality]))

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

      [{:ast/term :record :fields fields}
       ({:ast/type :existential-variable :id alpha} :as existential-type)
       _] ; ×Iα^
      (let [current (zip/node @(:type-checker/facts *context*))
            _       (swap! (:type-checker/facts *context*)
                      zip/focus-left
                      (get (existential-facts) alpha))
            type
            (find-record module fields)]
        (swap! (:type-checker/facts *context*) zip/focus-right current)
        (instantiate-to module existential-type [type :kind/type])
        (analysis:check module term [(denominalize module type) principality]))

      [{:ast/term :sequence :operations operations} _ _]
      (let [return (last operations)]
        (some->> operations
          (butlast)
          (run!
            (fn [operation]
              (let [alpha (fresh-existential)]
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
  [& vars]
  (let [get-existential
        (some-fn
          :fact/declare-existential
          :fact/restrict-existential
          :fact/solve-existential)]
    (->> *context*
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
  {:pre [(ast/module? module)]}
  (match [type [solution kind]]
    [({:ast/type :existential-variable :id alpha} :as alpha-type)
     [({:ast/type :existential-variable :id beta} :as beta-type) _]]
    (cond
      (and (unsolved? beta) (before? alpha beta)) ; InstReach
      (solve-existential beta alpha-type kind)

      (and (restricted? beta) (before? alpha beta)) ; InstReach⊃
      (do (restrict-existential alpha (get (existential-restrictions) beta))
          (clear-existential beta)
          (solve-existential beta alpha-type kind))

      :else ; InstSolve
      (solve-existential alpha beta-type kind))

    [{:ast/type :existential-variable :id alpha}
     [{:ast/type :variant :injectors injectors} _]] ; InstBin
    (let [_ (swap! (:type-checker/facts *context*)
              zip/focus-left
              {:fact/declare-existential alpha :kind :kind/type})
          injectors*
          (->> injectors
            (map (fn [[injector value]]
                   [injector (when (some? value) (fresh-existential))]))
            (into (empty injectors)))]
      (solve-existential alpha (assoc solution :injectors injectors*))
      (swap! (:type-checker/facts *context*) zip/->end)
      (dorun (map
               (fn [alpha-n tau-n]
                 (when (some? alpha-n)
                   (instantiate-to module alpha-n [(apply tau-n) :kind/type])))
               (vals injectors*)
               (vals injectors))))

    [{:ast/type :existential-variable :id alpha}
     [{:ast/type :record :fields fields} _]] ; InstBin
    (let [_ (swap! (:type-checker/facts *context*)
              zip/focus-left
              {:fact/declare-existential alpha :kind :kind/type})
          fields* ; TODO(tjgr): recycle exvars
          (->> fields
            (map (fn [[field value]]
                   [field (fresh-existential)]))
            (into (empty fields)))]
      (solve-existential alpha (assoc solution :fields fields*))
      (swap! (:type-checker/facts *context*) zip/->end)
      (dorun (map
               (fn [alpha-n tau-n]
                 (when (some? alpha-n)
                   (instantiate-to module alpha-n [(apply tau-n) :kind/type])))
               (vals fields*)
               (vals fields))))

    [{:ast/type :existential-variable :id alpha}
     [{:ast/type :function :domain domain :return return} _]] ; InstBin
    (let [_ (swap! (:type-checker/facts *context*)
              zip/focus-left
              {:fact/declare-existential alpha :kind :kind/type})
          domain* (when-not (type/existential-variable? domain) (fresh-existential))
          return* (when-not (type/existential-variable? return) (fresh-existential))]
      (solve-existential alpha
        {:ast/type :function
         :domain (or domain* domain)
         :return (or return* return)})
      (swap! (:type-checker/facts *context*) zip/->end)
      (when domain*
        (instantiate-to module domain* [(apply domain) :kind/type]))
      (when return* 
        (instantiate-to module return* [(apply return) :kind/type])))

    [({:ast/type :existential-variable :id alpha}
      :guard #(->> % :id (contains? (existential-restrictions))))
     _] ; InstSolve⊃
    (let [restrictions (get (existential-restrictions) alpha)]
      (doseq [restriction restrictions]
        (->> restriction
          (walk/prewalk-replace {type solution})
          (proposition:instance module))
        (solve-existential alpha solution kind)))

    [{:ast/type :existential-variable :id alpha} _]
    (let [solution (apply solution)]
      ;; TODO: check for wellformedness
      (solve-existential alpha solution kind)))) ; InstSolve

(defn- subtyping:equivalent
  "Γ ⊢ A ≡ B ⊣ Δ
  found on pg. 42"
  [module type-a type-b]
  {:pre [(type/is? type-a)
         (type/is? type-b)]}
  (let [type-a (apply type-a)
        type-b (apply type-b)]
    (when (not= type-a type-b)
      (match [type-a type-b]
        [{:ast/type :function :domain a-1 :return a-2}
         {:ast/type :function :domain b-1 :return b-2}] ; ≡⊕F
        (do (subtyping:equivalent module a-1 b-1)
            (subtyping:equivalent module (apply a-2) (apply b-2)))

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
                  (subtyping:equivalent module (apply type-1) (apply type-2))))
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
                (subtyping:equivalent module (apply type-1) (apply type-2)))
              fields-1
              fields-2)
            nil)

        [{:ast/type :forall :variable alpha-1 :body body-1}
         {:ast/type :forall :variable alpha-2 :body body-2}] ; ≡∀
        (if-not (= alpha-1 alpha-2)
          (throw (ex-info "Different universal variables cannot be equivalent"
                   {:left type-a :right type-b}))
          (let [mark (fresh-mark)]
            (declare-universal alpha-1 :kind/type)
            (subtyping:equivalent module body-1 body-2)
            (drop mark)))

        [{:ast/type :guarded :proposition p :body a}
         {:ast/type :guarded :proposition q :body b}] ; ≡⊃
        (do (proposition:equivalent module p q)
            (subtyping:equivalent module
              (apply a)
              (apply b)))

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
        (do
          (cond
            (not= (count parameters-1) (count parameters-2))
            (let [[longer shorter] (sort-by count [type-a type-b])
                  [inner outer] (split-at (count (:parameters shorter)) (:parameters longer))]
              (subtyping:equivalent module
                                    (assoc longer :parameters inner)
                                    (:operator shorter))
              (mapv (partial subtyping:equivalent module)
                    outer
                    (:parameters shorter)))

            (= (count parameters-1) (count parameters-2))
            (do
              (subtyping:equivalent module operator-1 operator-2)
              (mapv
               (fn [param-1 param-2] (subtyping:equivalent module param-1 param-2))
               parameters-1
               parameters-2)))
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
    (let [mark              (fresh-mark)
          existential-alpha (fresh-existential)]
      (subtype module :negative
        (walk/prewalk-replace {universal-alpha existential-alpha} a)
        b)
      (drop mark)
      nil)

    [:negative a {:ast/type :forall :variable universal-beta :body b}] ; <:∀R
    (let [mark (fresh-mark)]
      (subtype module :negative a b)
      (drop mark)
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
  (let [return [(apply return-type) return-principality]]
    (doseq [branch branches] ; MatchSeq + MatchEmpty
      (let [pattern      (first (:patterns branch))
            pattern-type (first pattern-types)]
        (annotate-pattern pattern pattern-type)
        (match [pattern
                (some->> pattern-type apply)
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
            (solve-existential alpha type)
            (match:check module
              [branch]
              [(cons type (next pattern-types)) :principal]
              return))

          [{:ast/pattern :variant :injector injector :value value}
           {:ast/type :variant :injectors injectors}
           _]                           ; Match+ₖ
          (let [injectors (->> injectors
                               (map (fn [[k v]] [(:db/id k) v]))
                               (into (empty injectors)))
                injector (:eid injector)]
            (cond
              (and (some? value)
                   (some? (get injectors injector)))
              (match:check module
                           [(update branch :patterns (fn [patterns] (cons value (next patterns))))]
                           [(cons (get injectors injector) (next pattern-types)) pattern-principality]
                           return)

              (and (nil? value)
                   (nil? (get injectors injector ::nf)))
              (match:check module
                           [(update branch :patterns next)]
                           [(next pattern-types) pattern-principality]
                           return)

              :else
              (do (undefined ::unknown-injector)
                  (throw (ex-info "Unknown injector" {:injector injector})))))

          [{:ast/pattern :variant :injector injector}
           {:ast/type :existential-variable :id alpha} ; Match+ₖα^
           :non-principal]
          (let [current (zip/node @(:type-checker/facts *context*))
                _       (swap! (:type-checker/facts *context*)
                          zip/focus-left
                          {:fact/declare-existential alpha
                           :kind                     :kind/type})
                type
                (find-variant module injector)]
            (swap! (:type-checker/facts *context*) zip/focus-right current)
            (solve-existential alpha type)
            (match:check module
              branches
              [(cons (denominalize module type) (next pattern-types)) pattern-principality]
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

            :else
            (throw (ex-info "Unknown record"
                     {:fields (keys pattern-fields)
                      :module (:name module)})))

          [{:ast/pattern :record :fields fields}
           {:ast/type :existential-variable :id alpha} ; Match×α^
           :non-principal]
          (let [current (zip/node @(:type-checker/facts *context*))
                _       (swap! (:type-checker/facts *context*)
                          zip/focus-left
                          {:fact/declare-existential alpha
                           :kind                     :kind/type})
                type
                (find-record module fields)]
            (swap! (:type-checker/facts *context*) zip/focus-right current)
            (solve-existential alpha type)
            (match:check module
              branches
              [(cons (denominalize module type) (next pattern-types)) pattern-principality]
              return))

          [{:ast/pattern :symbol :symbol symbol}
           pattern-type
           _]                           ; MatchNeg
          (let [mark (fresh-mark)]
            (bind-symbol symbol pattern-type :principal)
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

          ;; TODO: denominalize
          [_
           {:ast/type :application :operator operator :parameters parameters}
           _] 
          (let [type (-> module
                       (lookup-type (:name operator))
                         (type/instantiate-universals parameters))]
            (annotate-pattern pattern pattern-type)
            (match:check module
                         branches
                         [(cons type (next pattern-types)) pattern-principality]
                         return))

          [_
           {:ast/type :named :name name}
           _]
          (let [type (lookup-type module name)]
            (annotate-pattern pattern pattern-type)
            (match:check module
              branches
              [(cons type (next pattern-types)) pattern-principality]
              return))

          [-pattern -type -principality]
          (do (throw (ex-info "no matching pattern found"
                              {:pattern -pattern
                               :type -type
                               :principality -principality}))))))))

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
    (let [existential-variable (fresh-existential)
          body (walk/prewalk-replace {universal-variable existential-variable} body)]
      (equate-universal (:id universal-variable) existential-variable)
      (apply-spine module arguments [body principality]))

    [_ {:ast/type :guarded :proposition proposition :body body} _] ; ⊃Spine
    (do (proposition:true module proposition)
        (apply-spine module
          arguments
          [(apply body) principality]))

    [[e & s] {:ast/type :function :domain domain :return return} _] ; →Spine
    (do (analysis:check module e [domain principality])
        (apply-spine module
          s
          [(apply return) principality]))

    [_ {:ast/type :existential-variable :id alpha} :non-principal] ; α^Spine
    (let [current  (zip/node @(:type-checker/facts *context*))
          _        (swap! (:type-checker/facts *context*)
                     zip/focus-left
                     {:fact/declare-existential alpha
                      :kind                     :kind/type})
          alpha-1  (fresh-existential)
          alpha-2  (fresh-existential)
          function {:ast/type :function
                    :domain   alpha-1
                    :return   alpha-2}]
      (solve-existential alpha function :kind/type)
      (swap! (:type-checker/facts *context*) zip/focus-right current)
      (apply-spine module
        arguments
        [function principality]))))

(defn- recovering-apply-spine
  "Γ ⊢ s : A p ≫ C ⌈q⌉ ⊣ Δ
  found on pg. 37"
  [module arguments [type principality]]
  (let [[type principality*]
        (apply-spine module arguments [type principality])
        type (apply type)]
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

;; FIXME
(defn- to-jvm-type
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

;; FIXME
(defn- from-jvm-type
  [type]
  (undefined ::from-jvm-type)

  type

  (get
    (merge
      (->> {Boolean    "Bool"
            String     "String"
            BigInteger "Integer"
            Void/TYPE  "Unit"}
        (map (fn [[k v]] [k {:ast/type :named
                             :name     {:ast/reference :type
                                        :name      v
                                        :in        {:ast/reference :module :name ["lang" "builtin"]}}}]))
        (into {}))
      (->> {Boolean/TYPE :bool
            Integer/TYPE :int
            Object       :object}
        (map (fn [[k v]] [k {:ast/type  :primitive
                             :primitive v}]))
        (into {})))
    type))

(defn expand-macro
  [module term]
  (match term
         {:ast/term :application :function {:symbol symbol} :db/id id}
         (let [{:keys [expand]} (lookup-macro module symbol)
               ;; TODO: maybe freshen db/ids
               retractions (mapv (fn [[k v]] [:db/retract id k v]) (dissoc term :db/id))
               term (assoc (expand term) :db/id id)]
           (db/tx! db/state
                   (conj retractions term)
                   {:lang.compiler/pass ::expand-macro})
           term)))

(defn- reflect
  [class]
  (letfn [(name->class [name]
            (or
              (get
                (->> [Boolean/TYPE
                      Byte/TYPE
                      Character/TYPE
                      Double/TYPE
                      Float/TYPE
                      Integer/TYPE
                      Long/TYPE
                      Short/TYPE
                      Void/TYPE]
                  (map #(vector (.getName %) %))
                  (into {}))
                name)
              (when-let [[_ inner-class] (re-matches #"^(.+)<>$" name)]
                (-> inner-class
                  (name->class)
                  (make-array 0)
                  (clojure.core/type)))
              (Class/forName name)))
          (symbol->class [sym]
            (some-> sym (name) (name->class)))
          (symbols->classes [syms]
            (->> syms (map symbol->class) (into (empty syms))))]
    (-> class
      (clojure.reflect/reflect)
      (update :bases symbols->classes)
      (update :members
        #(->> %
           (map
             (fn [member]
               (-> member
                 (update :name name)
                 (update :type symbol->class)
                 (update :return-type symbol->class)
                 (update :declaring-class symbol->class)
                 (update :parameter-types symbols->classes)
                 (update :exception-types symbols->classes))))
           (into (empty %)))))))

(defn- macro?
  [term]
  (match term
         {:ast/term :symbol :symbol ref}
         (->> ref
              (db/->entity @db/state)
              :ast/definition
              (= :macro))))

(defn synthesize
  "Γ ⊢ e ⇒ A p ⊣ Δ
  found on pg. 37"
  [module term]
  (let [mark (fresh-mark)
        ;; _ (when (-> term :ast/term (= :application)) (undefined ::application))

        _
        [term
         (db/touch (db/->entity @db/state (:symbol (:function term))))]

        [type principality]
        (match term
          {:ast/term :symbol :symbol (symbol :guard jvm/native?)}
          (let [class (->> symbol :in :name
                        (str/join ".")
                        (java.lang.Class/forName)
                        (reflect))
                field (->> class
                        :members
                        (filter #(= (:name symbol) (:name %)))
                        (first))]
            [{:ast/type :object :class (:type field)}
             :principal])

          {:ast/term :symbol :symbol symbol}
          (lookup-binding module symbol)

          {:ast/term :application
           :function (_ :guard macro?)}
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
          (let [object-type
                (->> object
                     (synthesize module)
                     (first)
                     (to-jvm-type module))
                object-bases
                (->> object-type
                     (bases)
                     (map (fn [class] (symbol (. class Class/getName))))
                     (set))
                parameter-types
                (mapv
                 #(->> %
                       (synthesize module)
                       (first)
                       (to-jvm-type module))
                 arguments)
                member
                (->> function :symbol :in :name
                     (str/join ".")
                     (java.lang.Class/forName)
                     (reflect)
                     :members
                     (filter
                      (fn [member]
                        (and
                         #_(contains? (conj object-bases object-type) (:declaring-class member))
                         (contains? (:flags member) :public)
                         (= (:name member) (:name (:symbol function)))
                         (jvm/subclass?
                          (:parameter-types member)
                          (cond->> parameter-types
                            (contains? (:flags member) :static)
                            (into [object-type]))))))
                     (sort-by :parameter-types jvm/subclass?)
                     (last))]
            (annotate-term function
                           {:ast/type  :method
                            :class     object-type
                            :type (if (contains? (:flags member) :static) :static :instance)
                            :signature (conj (:parameter-types member) (:return-type member))})
            [(from-jvm-type (:return-type member)) :principal])

          {:ast/term :access
           :object   object} ; static field
          (undefined ::static-field)

          {:ast/term :quote
           :body     body}
          (let [alpha (fresh-existential)]
            (analysis:check module body [alpha :non-principal])
            [{:ast/type :quote :inner (get (existential-solutions) alpha alpha)} :non-principal])

          {:ast/term :unquote
           :body body}
          (let [[type principality] (synthesize module body)]
            (match type
              {:ast/type :quote :inner inner}
              [inner principality]))

          {:ast/term _}
          (let [alpha (fresh-existential)]
            (analysis:check module term [alpha :non-principal])
            [(get (existential-solutions) alpha alpha) :non-principal]))]
    (annotate-term term type)
    [(generalize mark type) principality]))

(defn- init
  [module]
  {:pre [(ast/module? module)]}
  (merge module
         {:definitions []}))

(defn- finalize
  [module]
  (assoc module :type-checker/instance-chains
         @(:type-checker/instance-chains *context*)))

(ns-unmap *ns* 'typecheck-definition)

(defmulti typecheck-definition (fn [_module definition] (:ast/definition definition)))

(defmethod typecheck-definition :type
  [_module {:keys [params body] :as definition}]
  (letfn [(universally-quantify-parameters [params type]
            (let [param-type->universal-variable
                  (->> params
                    (mapv (fn [param]
                            (-> (fresh-universal)
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
    [(->> body (universally-quantify-parameters params) apply)]))

(defmethod typecheck-definition :typeclass/declaration
  [module {:keys [db/id params members] :as definition}]
  (letfn [(universally-quantify-unknowns [type]
            (let [named-types   (->> type
                                     (type/nodes)
                                     (keep #(when (-> % :ast/type (= :named))
                                              (:name %)))
                                     (set))
                  known-types   (set/union
                                 (set (map #(-> % :db/id db/->ref) params))
                                 (set (vals (surface-types module)))
                                 (set (vals (imported-types module))))
                  unknown-types (set/difference named-types known-types)]
              (->> unknown-types
                   (reverse)
                   (reduce
                    (fn [inner unknown-type]
                      (let [universal (assoc (fresh-universal) :reference unknown-type)]
                        {:ast/type :forall
                         :variable universal
                         :body (walk/prewalk-replace
                                {{:ast/type :named :name unknown-type} universal}
                                inner)}))
                    type))))
          (universally-quantify-parameters [params type]
            (let [param-type->universal-variable
                  (->> params
                       (mapv (fn [{:keys [db/id]}]
                               (-> module
                                   (fresh-universal)
                                   (assoc :reference (db/->ref id)))))
                       (zipmap (map :db/id params)))]
              (->> params
                   (reverse)
                   (reduce
                    (fn [type {:keys [db/id]}]
                      {:ast/type :forall
                       :variable (get param-type->universal-variable id)
                       :body     type})
                    type)
                   (walk/prewalk
                    (fn [node] (or (and (= :named (ast/type? node))
                                        (get param-type->universal-variable (:eid (:name node))))
                                   node))))))]
    (map (fn [member]
           (merge member
                  {:type
                   (->> {:ast/type :guarded
                         :proposition
                         {:ast/constraint :instance
                          :typeclass      (db/->ref id)
                          :parameters
                          (mapv (fn [{:keys [db/id]}] {:ast/type :named :name (db/->ref id)}) params)}
                         :body (universally-quantify-unknowns (:type member))}
                        (universally-quantify-parameters params)
                        apply)}))
         members)))

(defmethod typecheck-definition :typeclass/instance
  ;; Check an instance declaration against the typeclass.

  ;; Checks the instance declaration against the type resulting from
  ;; substituting the specified parameters into the typeclass
  ;; declaration.

  ;; Takes care of detecting universal parameters and enforcing
  ;; superclass constraints.
  [module {:keys [typeclass superclasses types members db/id] :as definition}]
  (letfn [(strip-guard [type]
            (match type
              {:ast/type :guarded :body body} body
              _ (throw (ex-info "Unguarded type" {:type type}))))
          (abstract-parameters [types]
            ;; detect typeclass parameters that aren't real types, such as T in (Show (Option T)) and replace them with universal variables
            (let [substitutions
                  (reduce
                    (fn [type->universal-variable type]
                      (->> type
                        (type/nodes)
                        (distinct)
                        (keep (fn [type]
                                (match type
                                  {:ast/type :named :name ref}
                                  (when (and
                                         (not (definition/type? (db/->entity @db/state ref)))
                                         (not (contains? type->universal-variable type)))
                                    [type (assoc (fresh-universal) :reference type)])

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
                {:db/id (gensym)
                 :ast/type :guarded
                 :proposition superclass
                 :body type})
              type
              superclasses))
          (introduce-universal-parameters [type universal-variables]
            ;; introduces `:forall`s for any universal parameters, as detected by `abstract-parameters`
            (reduce
              (fn [type universal]
                {:db/id (gensym)
                 :ast/type :forall
                 :variable universal
                 :body type})
              type
              universal-variables))]
    (if-let [typeclass (db/touch (db/->entity @db/state typeclass))]
      (let [[types variables] (->> types
                                   (mapv apply)
                                   (abstract-parameters))
            superclasses      (->> superclasses
                                (map
                                  (fn [[typeclass parameters]]
                                    {:ast/constraint :instance
                                     :typeclass      typeclass
                                     :parameters     parameters}))
                                (into #{})
                                (walk/postwalk-replace variables)
                                (not-empty))]
        ;; DONE: transact primary instance onto `typeclass` entity
        ;; TODO: temporarily transact superclass instances onto `typeclass` entities
        ;; TODO: nested transaction with rollback in case of conflict
        (db/tx! db/state [(update typeclass :instances (fnil conj #{}) (db/->ref id))]
                {:lang.compiler/pass ::instantiate-typeclass})
        ;; (undefined ::instantiate-typeclass)

        (for [{:keys [body member] :as definition} members]
          (let [type
                (->
                 (db/->entity @db/state member)
                 :type
                 (type/instantiate-universals types)
                 (strip-guard)
                 (constrain-superclasses superclasses)
                 (introduce-universal-parameters (vals variables))
                 apply)]
            (analysis:check module body [type :principal])
            (merge definition {:type (apply type)}))))
      (do (undefined ::unknown-typeclass)
          (throw (ex-info "Unknown typeclass" {:typeclass name}))))))

(defmethod typecheck-definition :constant
  [module {:keys [body] :as definition}]
  (let [type (synthesize module body)]
    [(merge definition {:type type})]))

(defmethod typecheck-definition :macro
  [module {:keys [arguments body] :as definition}]
  ;; TODO: construct fn term with macro args, constrain args to quote<_>, synth macro type
  (let [mark        (fresh-mark)
        argument-types
        (mapv (fn [argument]
                (let [alpha (fresh-existential :kind/type)
                      type  {:ast/type :quote :inner alpha}]
                  (bind-symbol argument type :non-principal)
                  type))
              arguments)
        return-type (fresh-existential)
        _           (analysis:check module body [return-type :non-principal])
        type        [(->> return-type
                          (conj argument-types)
                          (type/function)
                          (generalize mark))
                     :non-principal]]
    [(merge definition
            {:type   type
             :expand (fn [application]
                       (let [environment (zipmap (map :db/id arguments)
                                                 (:arguments application))]
                         (interpreter/run environment body)))})]))

(defn run
  [module]
  {:pre [(ast/module? module)]}
  (log/debug "type-checking" (definition/name module))
  (binding [*context* #:type-checker{:current-variable (atom 0)
                                     :facts            (atom zip/empty)
                                     :instance-chains  (atom {})}]
    (->> module
         :definitions
         (reduce
          (fn [module definition]
            (reset! (:type-checker/facts *context*) zip/empty)
            (db/tx! db/state
                    (typecheck-definition module definition)
                    {:lang.compiler/pass [::run (:ast/definition definition)]})
            (swap! (:type-checker/instance-chains *context*)
                   (fn [chains]
                     (walk/prewalk
                      #(if (-> % :ast/type (= :existential-variable))
                         (apply %)
                         %)
                      chains)))
            module)
          (init module))
         (finalize))
    {:db-after @db/state}))

(comment

  (let [module {:ast/reference :module :name ["lang" "core"]}]
    (#'lang.compiler/init)
    (-> (#'lang.compiler/run module)
        #_#_(dissoc :macros :typeclasses)
        (update :definitions (partial filterv #(-> % :ast/definition #{:typeclass/declaration :typeclass/instance})))))

  (com.gfredericks.debug-repl/unbreak!!)

  (com.gfredericks.debug-repl/unbreak!)


  (db/touch (db/->entity @db/state 2))


  (throw (ex-info "foo" {}))

  (db/datoms @db/state)

  )
