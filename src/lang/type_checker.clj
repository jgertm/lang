(ns lang.type-checker
  (:refer-clojure :exclude [apply drop type])
  (:require [clojure.core.match :refer [match]]
            clojure.reflect
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [lang.interpreter :as interpreter]
            [lang.jvm :as jvm]
            [lang.module :as module]
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

(defn- existential-solutions
  [module]
  (->> module
    :type-checker/facts
    (deref)
    (zip/left-seq)
    (keep (fn [fact]
            (when-let [existential (:fact/solve-existential fact)]
              [existential (:type fact)])))
    (into {})))

(defn- local-bindings
  [module]
  (->> module
    :type-checker/facts
    (deref)
    (zip/left-seq)
    (keep (fn [fact]
            (when-let [symbol (:fact/bind-symbol fact)]
              [symbol {:type ((juxt :type :principality) fact)}])))
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
                 (get (all-bindings module))
                 :type)]
      [(apply module type) principality])
    (throw (ex-info "Unknown binding"
             {:symbol symbol
              :module (:name module)}))))

(defn- lookup-type
  [module name]
  (let [local-name   (if-not (:in name)
                       (assoc name :in (:name module))
                       name)
        builtin-name (assoc name :in {:reference :module :name ["lang" "builtin"]})
        types        (module/all-types module)]
    (or
      (get types local-name)
      (get types builtin-name)
      (undefined ::lookup-type)
      (throw (ex-info "Unknown type"
               {:type     name
                :module   (:name module)
                :in-scope (keys types)})))))

(defn- lookup-macro
  [module name]
  (get (module/all-macros module) name))

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
    type

    {:ast/type :forall}
    (update type :body (partial apply module))

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
         (into {})))

    {:ast/type :record}
    (update type :fields
      #(->> %
         (map (fn [[field content]] [field (apply module content)]))
         (into {})))

    {:ast/type :primitive}
    type

    {:ast/type :named :name name}
    (cond-> type
      (not (:in name)) (assoc-in [:name :in] {:reference :module :name ["lang" "builtin"]}))

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
   (let [current-fact
         (if-let [existing-solution (get (existential-solutions module) existential)]
           {:fact/solve-existential existential
            :kind                   kind
            :type                   existing-solution}
           {:fact/declare-existential existential :kind kind})
         new-solution
         {:fact/solve-existential existential
          :kind                   kind
          :type                   (apply module solution)}]
     (swap! (:type-checker/facts module)
       #(walk/prewalk-replace ; FIXME
          {current-fact new-solution}
          %)))
   nil))

(defn- find-variant
  [module injector]
  (let [type
        (-> module
          (module/all-injectors)
          (get injector))
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
  (walk/prewalk
    (fn resolve-node [node]
      (cond
        (some-> node :type-checker.macro/expands-to (realized?))
        (recur (-> node
                 :type-checker.macro/expands-to
                 (deref)
                 (assoc :type-checker.macro/expanded-from
                   (dissoc node :type-checker.macro/expands-to :type-checker.term/type))))

        (some-> node :type-checker.term/type (realized?))
        (update node :type-checker.term/type (comp (partial apply module) deref))

        (:type-checker.term/type node)
        (dissoc node :type-checker.term/type)

        (some-> node :type-checker.pattern/type (realized?))
        (update node :type-checker.pattern/type (comp (partial apply module) deref))

        (:type-checker.pattern/type node)
        (dissoc node :type-checker.pattern/type)

        :else node))
    definition))

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
          (fn [universal-variables fact]
            (match fact
              {:fact/declare-existential existential-variable-id :kind kind}
              (when-let [existential-variable (type/contains? type
                                                {:ast/type :existential-variable
                                                 :id       existential-variable-id})]
                (let [universal-variable (fresh-universal module)]
                  (swap! (:type-checker/facts module)
                    #(-> %
                       (zip/insert-right {:fact/solve-existential existential-variable-id
                                          :kind                   kind
                                          :type                   universal-variable})
                       (zip/right)))
                  (conj universal-variables universal-variable)))

              fact
              (do (swap! (:type-checker/facts module)
                    #(-> %
                       (zip/insert-right fact)
                       (zip/right)))
                  universal-variables)))
          []
          discard)
        generalized-type
        (->> universal-variables
          (reverse)
          (reduce
            (fn [type universal-variable]
              {:ast/type :forall
               :variable universal-variable
               :body     type})
            type)
          (apply module))]
    generalized-type))

(defn- synthesize-atom
  [atom]
  (or
    (get
      (->> module/builtins
        (map (fn [[k v]] [(:primitive v) {:ast/type :named :name k}]))
        (into {}))
      (:atom atom))
    (throw (ex-info "Unrecognized atom" {:atom atom}))))

(defn- analysis:check
  [module term [type principality]]
  (let [type (apply module type)]
    (match [term type principality]
      [{:ast/term :recur :reference reference :body body}
       _ _] ; Rec
      (let [mark (fresh-mark module)]
        (bind-symbol module reference type principality)
        (analysis:check module body [type principality])
        (drop module mark))  

      [{:ast/term :atom :atom atom}
       {:ast/type :existential-variable :id alpha}
       :non-principal] ; 1Iα^
      (let [type (synthesize-atom atom)]
        (annotate-term term type)
        (solve-existential module alpha type))

      [{:ast/term :atom :atom atom}
       {:ast/type :primitive :primitive primitive}
       :principal]
      (when-not (= (:atom atom) primitive)
        (throw (ex-info "Distinct types" {:atom atom :primitive primitive})))

      [{:ast/term :variant :variant {:injector injector :value value}}
       {:ast/type :variant :injectors injectors}
       _]
      (do
        (cond
          (and (some? value) (some? (get injectors injector))) ; variant wraps value
          (analysis:check module value [(get injectors injector) principality])

          (and (nil? value) (contains? injectors injector)) ; variant is enum
          nil

          :default ; injector isn't defined
          (throw (ex-info "Unknown injector" {:injector injector})))
        (annotate-term term (apply module type)))

      [{:ast/term :variant :variant {:injector injector}}
       {:ast/type :existential-variable :id alpha}
       _]
      (let [current (zip/node @(:type-checker/facts module))
            _       (swap! (:type-checker/facts module)
                      zip/focus-left
                      {:fact/declare-existential alpha
                       :kind                     :kind/type})
            type
            (find-variant module injector)]
        (swap! (:type-checker/facts module) zip/focus-right current)
        (solve-existential module alpha type)
        (analysis:check module term [type principality]))

      [{:ast/term :record :fields fields}
       {:ast/type :record :fields rows}
       _]
      (do
        (cond
          (= (keys fields) (keys rows))
          (run! (fn [[field value]]
                  (let [row-type (get rows field)]
                    (analysis:check module value [row-type principality]))) fields)

          :else
          (undefined ::analysis:check.record-type))
        (annotate-term term (apply module type))) 

      [{:ast/term :lambda :argument argument :body body}
       ({:ast/type :existential-variable :id alpha} :as alpha-type)
       :non-principal]          ; TODO: guard that `alpha` is declared
      (let [current  (zip/node @(:type-checker/facts module))
            _        (swap! (:type-checker/facts module)
                       zip/focus-left
                       {:fact/declare-existential alpha :kind :kind/type})
            mark     (fresh-mark module)
            alpha-1  (or
                       (some->> argument :type (apply module))
                       (fresh-existential module))
            alpha-2  (fresh-existential module)
            function {:ast/type :function
                      :domain   alpha-1
                      :return   alpha-2}]
        (solve-existential module alpha function)
        (swap! (:type-checker/facts module) zip/focus-right current)
        (bind-symbol module argument alpha-1 :non-principal)
        (analysis:check module body [alpha-2 :non-principal])
        (solve-existential module alpha (generalize module mark function) :kind/type)
        (annotate-term term (apply module alpha-type))
        (annotate-term body (apply module alpha-2)))

      [{:ast/term :match :body body :branches branches} _ _]
      (let [[pattern-type pattern-principality] (synthesize module body)]
        (match:check module
          branches
          [[(apply module pattern-type)] pattern-principality]
          [(apply module type) principality])
        nil)

      [_ {:ast/type :application :operator operator :parameters parameters} _]
      (let [type* (-> module
                    (lookup-type (:name operator))
                    (type/instantiate-universals parameters))]
        (analysis:check module term [type* principality]))

      [_ _ _]
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

(defn- unsolved?
  [module existential-variable]
  (not (contains? (existential-solutions module) existential-variable)))

(defn- before?
  [module & vars]
  (->> @(:type-checker/facts module)
    (zip/left-seq)
    (reverse)
    (keep (fn [fact] ((set vars) (:fact/declare-existential fact))))
    (= vars)))

(defn- instantiate-to
  [module type [solution kind]]
  (match [type [solution kind]]
    [({:ast/type :existential-variable :id alpha} :as alpha-type)
     [({:ast/type :existential-variable :id beta} :as beta-type) _]]
    (if (and (unsolved? module beta) (before? module alpha beta))
      (solve-existential module beta alpha-type kind) ; InstReach
      (solve-existential module alpha beta-type kind)) ; InstSolve

    [{:ast/type :existential-variable :id alpha}
     [{:ast/type :variant :injectors injectors} _]] ; InstBin
    (let [_ (swap! (:type-checker/facts module)
              zip/focus-left
              {:fact/declare-existential alpha :kind :kind/type})
          injectors*
          (->> injectors
            (map (fn [[injector value]]
                   [injector (when (some? value) (fresh-existential module))]))
            (into {}))]
      (solve-existential module alpha (assoc solution :injectors injectors*))
      (swap! (:type-checker/facts module) zip/->end)
      (dorun (map
               (fn [alpha-n tau-n]
                 (when (some? alpha-n)
                   (instantiate-to module alpha-n [(apply module tau-n) :kind/type])))
               (vals injectors*)
               (vals injectors))))

    [{:ast/type :existential-variable :id alpha}
     [{:ast/type :function} _]] ; InstBin
    (undefined :instantiate-to/inst-bin.function)

    [{:ast/type :existential-variable :id alpha} _]
    (let [solution (apply module solution)]
      ;; TODO: check for wellformedness
      (solve-existential module alpha solution kind)))) ; InstSolve

(defn- subtyping:equivalent
  [module type-a type-b]
  (let [type-a (apply module type-a)
        type-b (apply module type-b)]
    (when (not= type-a type-b)
      (match [type-a type-b]
        [{:ast/type :function :domain a-1 :return a-2}
         {:ast/type :function :domain b-1 :return b-2}] ; ≡⊕
        (do (subtyping:equivalent module a-1 b-1)
            (subtyping:equivalent module (apply module a-2) (apply module b-2)))

        [{:ast/type :variant :injectors injectors-1}
         {:ast/type :variant :injectors injectors-2}] ; ≡⊕
        (do (when-let [diff (not-empty (utils/symmetric-difference (set (keys injectors-1)) (set (keys injectors-2))))]
              (throw (ex-info "Different variant types" {:left type-a :right type-b :diff diff})))
            (merge-with
              (fn [type-1 type-2]
                (when (and (some? type-1) (some? type-2))
                  (subtyping:equivalent module (apply module type-1) (apply module type-2))))
              injectors-1
              injectors-2)
            nil)

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
        (undefined :subtyping.equivalent/fallthrough)))))

(defn- subtype
  [module polarity type-a type-b]
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
  [module branches [pattern-types pattern-principality] [return-type return-principality]]
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
            (throw (ex-info "Unknown record" {:fields (keys pattern-fields)})))

          [{:ast/pattern :symbol :symbol symbol}
           pattern-type
           _]                           ; MatchNeg
          (let [mark (fresh-mark module)]
            (bind-symbol module symbol pattern-type :principal)
            (match:check module
              [(update branch :patterns next)]
              [(next pattern-types) pattern-principality]
              return)
            (drop module mark))

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

(defn- recover-spine
  [module arguments [type principality]]
  (match [arguments type]
    [[] _]
    [type principality]

    [[e & s] {:ast/type :function :domain domain :return return}]
    (do (analysis:check module e [domain principality])
        (recover-spine module s [(apply module return) principality]))

    [es {:ast/type :forall :variable universal-variable :body body}]
    (let [existential-variable (fresh-existential module)]
      (recover-spine module
        es
        [(walk/prewalk-replace {universal-variable existential-variable} body) principality]))

    [_ {:ast/type :existential-variable :id alpha}]
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
      (recover-spine module arguments [function principality]))

    [_ _] (undefined :recover-spine/fallthrough)))

    [_ _ _] ; SpinePass
    (undefined ::recover-spine.spine-pass)))

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
                           'void              "Unit"
                           'java.lang.String  "String"
                           'java.lang.Integer "Integer"
                           'java.lang.Boolean "Boolean"
                           'java.lang.Object  "Object")}})

(defn expand-macro
  [module term]
  (match term
    {:ast/term :application :function {:symbol symbol}}
    (let [macro (lookup-macro module symbol)
          term* ((:expand macro) term)]
      (deliver (:type-checker.macro/expands-to term) term*)
      term*)))

(defn- synthesize
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
            (recover-spine module arguments))

          {:ast/term :access
           :object   object
           :field    {:ast/term  :application
                      :function  function
                      :arguments arguments}} ; instance method
          (let [object-type           (->> object
                                        (synthesize module)
                                        (first)
                                        (to-jvm-type module))
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
                                                    (= (:declaring-class member) object-type)
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
        type* (->> type
                (generalize module mark)
                (apply module))]
    (annotate-term term type*)
    [type* principality]))

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
                (rseq)
                (reduce
                  (fn [type param]
                    {:ast/type :forall
                     :variable (get param-type->universal-variable {:ast/type :named :name param})
                     :body     type})
                  (walk/prewalk-replace param-type->universal-variable type)))))
          (qualify-references [type]
            (walk/prewalk
              (fn [node]
                (match node
                  {:ast/type :named}
                  (cond
                    (= name (:name node))
                    (update node :name assoc :in (:name module))

                    (contains? (module/all-types module) (:name node))
                    (undefined ::qualify-references))

                  _ node))
              type))]
    (->> body
      (universally-quantify-parameters params)
      (qualify-references)
      (apply module))))

(defn run
  [module]
  (reduce
    (fn [module definition]
      (let [definition* (setup-annotations definition)]
        (match definition*
          {:ast/definition :type :name name}
          (let [name        (assoc name :in (:name module))
                type*       (abstract-type module definition*)
                definition* (assoc definition* :body type*)]
            (-> module
              (update :definitions conj definition*)
              (assoc-in [:types name] type*)))

          {:ast/definition :constant :name name :body expr}
          (let [mark                (fresh-mark module)
                [type principality] (synthesize module expr)
                definition*         (resolve-annotations module definition*)
                discard             (drop module mark)]
            (swap! (:type-checker/facts module) zip/->end)
            (-> module
              (assoc-in [:values name :type] [type :principal])
              (update :definitions conj
                (assoc definition* :type-checker.term/type type))))

          {:ast/definition :macro    :name name
           :arguments      arguments :body body}
          (let [mark        (fresh-mark module)
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
              (update :definitions conj definition)
              (assoc-in [:macros name]
                {:type      type
                 :arguments arguments
                 :expand    (fn [application]
                              (let [environment (zipmap arguments (:arguments application))]
                                (interpreter/run environment body)))}))))))
    (merge module
      {:definitions                   []
       :type-checker/current-variable (atom 0)
       :type-checker/facts            (atom zip/empty)})
    (:definitions module)))
