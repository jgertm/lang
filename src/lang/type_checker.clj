(ns lang.type-checker
  (:refer-clojure :exclude [apply drop])
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [lang.utils :as utils :refer [undefined]]
            [lang.zip :as zip]))

(def ^:private builtins
  (let [unit   {:ast/type :primitive :primitive :unit}
        string {:ast/type :primitive :primitive :string}]
    {:type-checker/types
     {{:reference :type, :name "Unit"} unit
      {:reference :type, :name "String"} string}
     :type-checker/values {}}))

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
    {:fact/bind-symbol symbol
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

(defn- lookup-binding
  [module symbol]
  (let [definitions (->> module
                      :type-checker/values
                      (map (fn [[name type]] [name [type :principal]]))
                      (into {}))
        locals (->> module
                 :type-checker/facts
                 (deref)
                 (zip/left-seq)
                 (keep (fn [fact]
                         (when-let [symbol (:fact/bind-symbol fact)]
                           [symbol ((juxt :type :principality) fact)])))
                 (into {}))]
    (get (merge definitions locals) symbol)))

(defn- lookup-type
  [module name]
  (get (:type-checker/types module) name))

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

    {:ast/type :function}
    (-> type
      (update :domain (partial apply module))
      (update :return (partial apply module)))

    {:ast/type :variant}
    (update type :variants
      #(->> %
         (map (fn [[keyword type]] [keyword (apply module type)]))
         (into {})))

    {:ast/type :primitive}
    type

    {:ast/type :named :name name}
    (or
      (lookup-type module name)
      (undefined :apply/named.unknown))

    _
    (undefined :apply/fallthrough)))

(defn- solve-existential
  ([module existential solution]
   (solve-existential module existential solution :kind/type))
  ([module existential solution kind]
   (let [existing-solution {:fact/solve-existential existential
                            :kind                   kind
                            :type                   (get (existential-solutions module) existential)}
         new-solution      {:fact/solve-existential existential
                            :kind                   kind
                            :type                   (apply module solution)}]
     (swap! (:type-checker/facts module)
       #(walk/prewalk-replace ; FIXME
          {{:fact/declare-existential existential :kind kind} new-solution
           existing-solution                                  new-solution}
          %)))
   nil))

(defn- analysis:check
  [module term type-principality]
  (let [[type principality] (if-let [[type principality] (not-empty type-principality)]
                              [(apply module type) principality]
                              type-principality)]
    (match [term type principality]
      [{:ast/term :atom :atom atom}
       {:ast/type :existential-variable :id alpha}
       :non-principal]
      (let [type {:ast/type :primitive :primitive (:atom atom)}]
        (solve-existential module alpha type))

      [{:ast/term :lambda :argument argument :body body}
       {:ast/type :existential-variable :id alpha}
       :non-principal]          ; TODO: guard that `alpha` is declared
      (let [_        (swap! (:type-checker/facts module)
                       zip/focus-left
                       {:fact/declare-existential alpha :kind :kind/type})
            mark     (fresh-mark module)
            alpha-1  (fresh-existential module)
            alpha-2  (fresh-existential module)
            function {:ast/type :function
                      :domain   alpha-1
                      :return   alpha-2}]
        (solve-existential module alpha function)
        (swap! (:type-checker/facts module) zip/->end)
        (bind-symbol module argument alpha-1 :non-principal)
        (analysis:check module body [alpha-2 :non-principal])
        (let [discard (drop module mark)
              universal-variables
              (reduce
                (fn [universal-variables fact]
                  (match fact
                    {:fact/declare-existential existential-variable-id :kind kind}
                    (let [existential-variable  {:ast/type :existential-variable
                                                 :id       existential-variable-id}
                          universal-variable    (fresh-universal module)]
                      (swap! (:type-checker/facts module)
                        #(-> %
                           (zip/insert-right {:fact/solve-existential existential-variable-id
                                              :kind kind
                                              :type universal-variable})
                           (zip/right)))
                      (conj universal-variables universal-variable))

                    fact
                    (do (swap! (:type-checker/facts module)
                          #(-> %
                             (zip/insert-right fact)
                             (zip/right)))
                        universal-variables)))
                []
                discard)
              generalized-function
              (->> universal-variables
                (reduce
                  (fn [type universal-variable]
                    {:ast/type :forall
                     :variable universal-variable
                     :body     type})
                  function)
                (apply module))]
          (solve-existential module alpha generalized-function :kind/type)
          nil))

      [{:ast/term :match :body body :branches branches} _ _]
      (let [[pattern-type pattern-principality] (synthesize module body)]
        (match:check module
          branches
          [(apply module pattern-type) pattern-principality]
          [(apply module type) principality])
        nil)

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

(defn- universally-quantified?
  [type]
  (match type
    {:ast/type :forall} true
    _ false))

(defn- existentially-quantified?
  [type]
  (match type
    {:ast/type :exists} true
    _ false))

(defn- quantified?
  [type]
  (or
    (universally-quantified? type)
    (existentially-quantified? type)))

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
     [{:ast/type :variant :variants variants} _]] ; InstBin
    (let [_ (swap! (:type-checker/facts module)
              zip/focus-left
              {:fact/declare-existential alpha :kind :kind/type})
          variants* (->> #(fresh-existential module)
                      (repeatedly (count variants))
                      (zipmap (keys variants)))]
      (solve-existential module alpha {:ast/type :variant :variants variants*})
      (swap! (:type-checker/facts module) zip/->end)
      (dorun (map
               (fn [alpha-n tau-n]
                 (instantiate-to module alpha-n [(apply module tau-n) :kind/type]))
               (vals variants*)
               (vals variants))))

    [{:ast/type :existential-variable :id alpha}
     [{:ast/type :function} _]] ; InstBin
    (undefined :instantiate-to/inst-bin.function)

    [{:ast/type :existential-variable :id alpha} _]
    (let [solution (apply module solution)]
      ;; TODO: check for wellformedness
      (solve-existential module alpha solution kind)))) ; InstSolve

(defn- subtyping:equivalent
  [module type-a type-b]
  (match [type-a type-b]
    [{:ast/type :function :domain a-1 :return a-2}
     {:ast/type :function :domain b-1 :return b-2}] ; ≡⊕
    (do (subtyping:equivalent module a-1 b-1)
        (subtyping:equivalent module (apply module a-2) (apply module b-2)))

    [{:ast/type :existential-variable} _] ; ≡InstantiateL
    (instantiate-to module type-a [type-b :kind/type])
 
    [_ {:ast/type :existential-variable}] ; ≡InstantiateR
    (instantiate-to module type-b [type-a :kind/type])

    [{:ast/type :primitive :primitive primitive-1}
     {:ast/type :primitive :primitive primitive-2}]
    (if (not= primitive-1 primitive-2)
      (throw (ex-info "Type mismatch" {:left primitive-1 :right primitive-2}))
      nil)

    [_ _]
    (undefined :subtyping.equivalent/fallthrough)))

(defn- subtype
  [module polarity type-a type-b]
  (match [polarity type-a type-b]
    [_ (_ :guard (complement quantified?)) (_ :guard (complement quantified?))] ; <:Equiv
    (subtyping:equivalent module type-a type-b)

    [:negative {:ast/type :forall :variable universal-alpha :body a} (b :guard (complement universally-quantified?))] ; <:∀L
    (let [mark              (fresh-mark module)
          existential-alpha (fresh-existential module)]
      (subtype module :negative
        (walk/prewalk-replace {universal-alpha existential-alpha} a)
        b)
      (drop module mark)
      nil)

    [_ _ _] (undefined :subtype/fallthrough)))

(defn- find-variant
  [module tag]
  (letfn [(check [type]
            (match type
              {:ast/type :variant :variants variants}
              (contains? variants tag)

              {:ast/type :forall :body body}
              (check body)

              {:ast/type :primitive}
              nil

              _ (undefined :find-variant/fallthrough)))]
    (->> module
      :type-checker/types
      (vals)
      (some #(when (check %) %)))))

(defn- instantiate-universal
  [module type]
  (match type
    {:ast/type :forall :variable universal-variable :body body}
    (let [existential-variable (fresh-existential module)]
      [(->> body
         (instantiate-universal module)
         (first)
         (walk/prewalk-replace {universal-variable existential-variable}))
       :non-principal])

    _
    [type :principal]))

(defn- match:check
  [module branches pattern return]
  (let [apply*
        #(if-let [[type principality] (not-empty %)]
           [(apply module type) principality]
           %) 
        [pattern-type pattern-principality :as pattern] (apply* pattern)
        [return-type return-principality :as return]    (apply* return)]
    (match [branches pattern]
      [[{:pattern {:ast/pattern :variant :variant [tag sub-pattern]} :action action}]
       [{:ast/type :variant :variants variants} principality]]
      (match:check module [{:pattern sub-pattern :action action}] [(get variants tag) principality] return)

      [[{:pattern {:ast/pattern :variant :variant [tag _]}}]
       [{:ast/type :existential-variable :id alpha} :non-principal]]
      (let [current (zip/node @(:type-checker/facts module))
            _       (swap! (:type-checker/facts module)
                      zip/focus-left
                      {:fact/declare-existential alpha
                       :kind                     :kind/type})
            [type principality]
            (->> tag
              (find-variant module)
              (instantiate-universal module))]
        (swap! (:type-checker/facts module) zip/focus-right current)
        (solve-existential module alpha type)
        (match:check module branches [type principality] return))

      [[({:action action} :only [:action])]
       _]
      (analysis:check module action return)

      [([] :guard empty?)
       _]
      nil

      [[{:pattern {:ast/pattern :symbol :symbol symbol} :action action}]
       [type principality]]
      (let [mark (fresh-mark module)]
        (bind-symbol module symbol type principality)
        (match:check module [{:action action}] [] return)
        (drop module mark)
        nil)

      [[{:pattern {:ast/pattern :wildcard} :action action}]
       _]
      (match:check module [{:action action}] [] return)

      [[first-pattern & more-patterns]
       [pattern-type pattern-principality]]
      (do
        (match:check module [first-pattern] pattern return)
        (match:check module
          more-patterns
          [(apply module pattern-type) pattern-principality]
          [(apply module return-type) return-principality])))))

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
    (let [current (zip/node @(:type-checker/facts module))
          _        (swap! (:type-checker/facts module)
                     zip/focus-left
                     {:fact/declare-existential alpha
                      :kind                     :kind/type})
          alpha-1  (fresh-existential module)
          alpha-2  (fresh-existential module)
          function {:ast/type :function
                    :domain  alpha-1
                    :return  alpha-2}]
      (solve-existential module alpha function :kind/type)
      (swap! (:type-checker/facts module) zip/focus-right current)
      (recover-spine module arguments [function principality]))

    [_ _] (undefined :recover-spine/fallthrough)))

(defn- synthesize
  [module term]
  (let [[type principality]
        (match term
          {:ast/term :symbol :symbol symbol}
          (lookup-binding module symbol)

          {:ast/term :application :function function :arguments arguments}
          (let [[function-type function-principality] (synthesize module function)]
            (recover-spine module arguments [function-type function-principality]))

          {:ast/term _}
          (let [alpha (fresh-existential module)]
            (analysis:check module term [alpha :non-principal])
            [(get (existential-solutions module) alpha alpha) :non-principal]))

        _ (when (not (map? type)) (undefined :messed-up-type))

        type* (apply module type)]
    (deliver (:type-checker/type term) type*)
    [type* principality]))

(defn- abstract-type
  [module {:keys [params body] :as definition}]
  (let [param-type->universal-variable
        (->> #(fresh-universal module)
          (repeatedly (count params))
          (zipmap (map (fn [param] {:ast/type :named :name param}) params)))]
    (->> params
      (rseq)
      (reduce
        (fn [type param]
          {:ast/type :forall
           :variable (get param-type->universal-variable {:ast/type :named :name param})
           :body     type})
        (walk/prewalk-replace
          (merge (:type-checker/types module) param-type->universal-variable)
          body)))))

(defn- annotate-nodes
  [definition]
  (walk/prewalk
    (fn [node]
      (if (:ast/term node)
        (assoc node :type-checker/type (promise))
        node))
    definition))

(defn- resolve-nodes
  [definition]
  (walk/prewalk
    (fn [node]
      (cond
        (some-> node :type-checker/type (realized?))
        (update node :type-checker/type deref)

        (:type-checker/type node)
        (undefined :resolve-nodes/unrealized-type-promise)

        :else node))
    definition))

(defn run
  [module]
  (reduce
    (fn [module definition]
      (let [definition* (annotate-nodes definition)]
        (match definition*
          {:ast/definition :type :name name}
          (-> module
            (assoc-in [:type-checker/types name] (abstract-type module definition*))
            (update :definitions conj definition*))

          {:ast/definition :constant :name name :body expr}
          (let [mark                (fresh-mark module)
                [type principality] (synthesize module expr)
                discard             (drop module mark)] ; TODO: probably ok to throw away all facts
            (swap! (:type-checker/facts module) zip/->end)
            (-> module
              (assoc-in [:type-checker/values name] type)
              (update :definitions conj (resolve-nodes definition*))))

          {:ast/definition :native :name name :type type}
          (let [type* (apply module type)]
            (-> module
              (assoc-in [:type-checker/values name] type*)
              (update :definitions conj (assoc definition :type type*)))))))
    (merge module
      builtins
      {:definitions                   []
       :type-checker/current-variable (atom 0)
       :type-checker/facts            (atom zip/empty)})
    (:definitions module)))
