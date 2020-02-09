(ns lang.type-checker
  (:refer-clojure :exclude [apply drop])
  (:require [clojure.core.match :refer [match]]
            [clojure.pprint :refer [pprint]] ; TODO: rm
            [com.gfredericks.debug-repl :refer [break! unbreak! unbreak!!]]
            [lang.zip :as zip]
            [clojure.walk :as walk]))

(defn undefined []
  (throw (Exception. "not implemented yet")))

(declare analysis:check)
(declare match:check)
(declare synthesize)
(declare subtyping:join)
(declare subtyping:polarity)
(declare subtype)

(defn- fresh-existential
  ([module]
   (fresh-existential module :kind/type))
  ([module kind]
   (let [variable-id (swap! (:type-checker/current-variable module) inc)
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
   (let [variable-id (swap! (:type-checker/current-variable module) inc)
         type        {:ast/type :universal-variable
                      :id       variable-id}]
     (swap! (:type-checker/facts module)
       zip/insert-left
       {:fact/declare-universal variable-id
        :kind                   kind})
     type)))

(defn- fresh-mark
  [module]
  (let [marker {:fact/marker (swap! (:type-checker/current-variable module) inc)}]
    (swap! (:type-checker/facts module) zip/insert-left marker)
    marker))

(defn- drop
  [module fact]
  (swap! (:type-checker/facts module)
    #(-> %
       (zip/->end)
       (zip/focus-left fact)
       (zip/left)
       (zip/truncate-right)))
  nil)

(defn- solve-existential
  ([module existential solution]
   (solve-existential module existential solution :kind/type))
  ([module existential solution kind]
   (swap! (:type-checker/facts module)
     #(-> %
        (zip/focus-left {:fact/declare-existential existential :kind kind})
        (zip/replace {:fact/solve-existential existential
                      :kind                   kind
                      :type                   solution})))
   nil))

(defn- bind-symbol
  [module symbol type principality]
  (swap! (:type-checker/facts module)
    #(-> %
       (zip/insert-right {:fact/bind-symbol symbol
                          :type type
                          :principality     principality})
       (zip/right)))
  nil)

(defn- existential-solutions
  [module]
  (->> module
    :type-checker/facts
    (deref)
    (zip/left-seq)
    (keep (fn [fact]
            (when-let [existential (:fact/solve-existential fact)]
              [existential fact])))
    (into {})))

(defn- bindings
  [module]
  (->> module
    :type-checker/facts
    (deref)
    (zip/left-seq)
    (keep (fn [fact]
            (when-let [symbol (:fact/bind-symbol fact)]
              [symbol ((juxt :type :principality) fact)])))
    (into {})))

(defn- apply
  [module type]
  (match type ; TODO
    _ type))

(defn- analysis:check
  [module term [type principality]]
  (match [term type principality]
    [{:ast/term :atom :atom atom}
     {:ast/type :existential-variable :id alpha}
     :non-principal]
    (let [type {:ast/type :primitive :primitive (:atom atom)}]
      (solve-existential module alpha type))

    [{:ast/term :lambda :argument argument :body body}
     {:ast/type :existential-variable :id alpha}
     :non-principal] ; TODO: guard that `alpha` is declared
    (let [_        (swap! (:type-checker/facts module) zip/focus-left {:fact/declare-existential alpha :kind :kind/type})
          mark     (fresh-mark module)
          alpha-1  (fresh-existential module)
          alpha-2  (fresh-existential module)
          function {:ast/type :function
                    :domain   alpha-1
                    :return   alpha-2}]
      (solve-existential module alpha function)
      (bind-symbol module argument alpha-1 :non-principal)
      (analysis:check module body [alpha-2 :non-principal])

      (break! :analysis.check/function-introduction-existential)
      (-> module
        :type-checker/facts
        (deref)
        #_(zip/focus-left mark)
        #_(second)
        #_:right)
      alpha

      ;; TODO: shit
      (undefined))

    [{:ast/term :match :body body :branches branches} _ _]
    (let [[pattern-type pattern-principality] (synthesize module body)]
      (match:check module
        branches
        [(apply module pattern-type) pattern-principality]
        [(apply module type) principality])
      nil)

    [_ _ _]
    (let [[synth-type] (synthesize module term)
          polarity (subtyping:join
                     (subtyping:polarity type)
                     (subtyping:polarity synth-type))]
      (subtype module polarity synth-type type))))

(comment
  (lang.compiler/run "../lang/examples/option.lang")

  (unbreak!!)

  [#_module type-a type-b]

  @(:type-checker/facts module)

  (unbreak!)

  )

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

(defn- is-universally-quantified?
  [type]
  (match type
    {:ast/type :forall} true
    _ false))

(defn- is-existentially-quantified?
  [type]
  (match type
    {:ast/type :exists} true
    _ false))

(defn- is-quantified?
  [type]
  (or
    (is-universally-quantified? type)
    (is-existentially-quantified? type)))

(defn- instantiate-to
  [module type [solution kind]]
  (match [type [solution kind]]
    [{:ast/type :existential-variable} _]
    (let [solution (apply module solution)]
      ;; TODO: check for wellformedness
      (solve-existential module type solution))))

(defn- subtyping:equivalent
  [module type-a type-b]
  (match [type-a type-b]
    [_ {:ast/type :existential-variable}]
    (instantiate-to module type-b [type-a :kind/type])

    [_ _] (undefined)))

(defn subtype
  [module polarity type-a type-b]
  (match [polarity type-a type-b]
    [_ (_ :guard (complement is-quantified?)) (_ :guard (complement is-quantified?))]
    (subtyping:equivalent module type-a type-b)

    [_ _ _] (undefined)))

(defn- find-variant
  [module tag]
  (letfn [(check [type]
            (match type
              {:ast/type :variant :variants variants}
              (contains? variants tag)

              {:ast/type :forall :body body}
              (check body)))]
    (->> module
      :types
      (vals)
      (some #(when (check (:body %)) %)))))

(defn- instantiate-typedef
  [type module]
  (match (:body type)
    {:ast/type :forall :variable universal-variable :body body}
    (let [existential-variable (fresh-existential module)]
      [(walk/prewalk-replace {universal-variable existential-variable} body) :non-principal])

    _
    [type :principal]))

(defn- match:check
  [module branches [pattern-type pattern-principality :as pattern] return]
  (match [branches pattern]
    [[{:pattern {:ast/pattern :variant :variant [tag sub-pattern]} :action action}] [{:ast/type :variant :variants variants} principality]]
    (match:check module [{:pattern sub-pattern :action action}] [(get variants tag) principality] return)

    [[{:pattern {:ast/pattern :variant :variant [tag _]}}] [{:ast/type :existential-variable :id alpha} :non-principal]]
    (let [[type principality]
          (-> module
            (find-variant tag)
            (instantiate-typedef module))]
      (solve-existential module alpha type)
      (match:check module branches [type principality] return))

    [[({:action action} :only [:action])] _]
    (analysis:check module action return)

    [([] :guard empty?) _]
    nil

    [[{:pattern {:ast/pattern :symbol :symbol symbol} :action action}] [type principality]]
    (let [mark (fresh-mark module)]
      (bind-symbol module symbol type principality)
      (match:check module [{:action action}] [] return)
      (drop module mark))

    [[{:pattern {:ast/pattern :wildcard} :action action}] _]
    (match:check module [{:action action}] [] return)

    [[first-pattern & more-patterns] [pattern-type pattern-principality]]
    (do
      (match:check module [first-pattern] pattern return)
      (match:check module more-patterns [(apply module pattern-type) pattern-principality] return))))


(defn- synthesize
  [module term]
  (match term
    {:ast/term :symbol :symbol symbol}
    (get (bindings module) symbol)

    {:ast/term :application}
    (undefined)

    {:ast/term _}
    (let [alpha (fresh-existential module)
          _     (analysis:check module term [alpha :non-principal])]
      [(get (existential-solutions module) alpha alpha) :non-principal])))

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
           :variable (get param-type->universal-variable param)
           :body     type})
        (walk/prewalk-replace param-type->universal-variable body))
      (assoc definition :body))))

(defn run
  [module]
  (->> module
    :definitions
    (take 2) ; TODO: rm
    (reduce
      (fn [module definition]
        (match definition
          {:ast/definition :type :name name}
          (assoc-in module [:types name] 
            (abstract-type module definition))

          {:ast/definition :constant :name name :body expr}
          (let [[type principality] (synthesize module expr)]
            (assoc-in module [:values name] type))))
      (assoc module
        :type-checker/current-variable (atom 0)
        :type-checker/facts (atom zip/empty)))))
