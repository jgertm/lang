(ns lang.code-generator
  (:refer-clojure :exclude [load class type])
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [insn.core :as insn]
            insn.util
            [lang.jvm :as jvm]
            [lang.module :as module]
            [lang.utils :as utils :refer [undefined]])
  (:import [java.lang.invoke CallSite LambdaMetafactory MethodHandle MethodHandles$Lookup MethodType]))

(declare lookup-type)
(declare ->instructions)

(def ^:private bytecode-version 11)

(def ^:private VOID 'void)
(def ^:private INT Integer/TYPE)
(def ^:private BOOL Boolean/TYPE)

(defn- void?
  [type]
  (= type VOID))

(defn- class-name
  [module]
  (munge (str/join "." (:name (:name module)))))

(defn- subclass
  [module & classes]
  (munge
    (format "%s.%s"
      (class-name module)
      (str/join "$" (map :name classes)))))

(defn- load
  [type]
  (get {INT  :iload
        BOOL :iload} type :aload))

(defn- store
  [type]
  (get {INT  :istore
        BOOL :istore} type :astore))

(defn- return
  [type]
  (get {VOID :return
        INT  :ireturn
        BOOL :ireturn} type :areturn))

(defn- primitive?
  [type]
  (contains? #{VOID BOOL INT} type))

(defn- reference?
  [type]
  (not (primitive? type)))

(defn- wide?
  [type]
  (contains? #{:double :long} type))

(defn- next-register
  [module type]
  (let [fn (if (wide? type)
             (comp inc inc)
             inc)]
    {:register (swap! (:code-generator/next-register module) fn)
     :width    (fn 0)}))

(defn- next-mark
  [module]
  {:mark (swap! (:code-generator/next-mark module) inc)})

(defn- specialize-type
  [type]
  (match type
    {:ast/type :recur :body body}
    (recur body)

    {:ast/type :forall :variable variable :body body}
    (->> body
      (walk/prewalk-replace {variable {:ast/type :primitive :primitive :object}})
      (recur))

    _ type))

(defn- local-instructions
  [module]
  @(:code-generator/bindings module))

(defn- all-instructions
  [module]
  (merge
    (module/all-instructions module)
    (local-instructions module)))

(defn- lookup-binding
  [module symbol]
  (or
    (get (all-instructions module) symbol)
    (undefined ::lookup-binding)
    (throw (ex-info "Could not find instructions for binding"
             {:symbol symbol
              :module (:name module)}))))

(defn- add-binding
  [module symbol instructions]
  (swap! (:code-generator/bindings module)
    assoc (select-keys symbol [:reference :name :in]) instructions))

(defn- all-classes
  [module]
  (merge
    (module/all-classes module)
    @(:code-generator/classes module)))

(defn- lookup-injector
  [module injector]
  (or
    (get (all-classes module) injector)
    (undefined ::lookup-injector)))

(defn- add-injector
  [module injector outer inner]
  (swap! (:code-generator/classes module)
    assoc injector {:outer outer :inner inner}))

(defn- lookup-variant
  [module injectors]
  (or
    (get (all-classes module) (set injectors))
    (undefined ::lookup-variant)))

(defn- add-variant
  [module injectors class]
  (swap! (:code-generator/classes module)
    assoc (set injectors) class))

(defn- add-field
  [module field class]
  (swap! (:code-generator/classes module)
    assoc field class))

(defn- lookup-field
  [module field]
  (or
    (get (all-classes module) field)
    (undefined ::lookup-field)))

(defn- lookup-record
  [module fields]
  (or
    (some
      (fn [[name class]]
        (when (and (set? name) (set/subset? (set fields) name))
          class))
      (all-classes module))
    (undefined ::lookup-record)))

(defn- add-record
  [module fields class]
  (swap! (:code-generator/classes module)
    assoc (set fields) class))

(defn- lookup-method-type
  [module type]
  (match (specialize-type type)
    {:ast/type :function :domain domain :return return}
    (let [domain* (lookup-type module domain)
          return* (if (= :function (:ast/type return))
                    (lookup-method-type module return)
                    (lookup-type module return))]
      (if (vector? return*) ; FIXME: returns array
        (into [domain*] return*)
        (conj [domain*] return*)))))

(defn- lookup-type
  [module type]
  (let [primitives
        (->> module/builtins
          (map (comp (juxt :body :class) val))
          (into {}))]
    (-> type
      (specialize-type)
      (match
        {:ast/type :primitive}
        (get jvm/primitives type)

        {:ast/type :named :name name}
        (if-let [primitive (get module/builtins name)]
          (recur module primitive)
          (get (all-classes module) name))

        {:ast/type :application :operator operator}
        (if-not (= operator
                  {:ast/type :named
                   :name     {:reference :type
                              :name      "Array"
                              :in        {:reference :module :name ["lang" "builtin"]}}})
          (recur module operator)
          (let [[element] (:parameters type)]
            (Class/forName (format "[L%s;" (lookup-type module element)))))

        {:ast/type (:or :existential-variable :universal-variable)} ; FIXME: perhaps?
        Object

        {:ast/type :variant :injectors injectors}
        (or
          (lookup-variant module (keys injectors))
          (throw (ex-info "Could not find variant type" {:injectors injectors})))

        {:ast/type :record :fields fields}
        (or
          (lookup-record module (keys fields))
          (undefined ::lookup-type.record)
          (throw (ex-info "Could not find record type" {:fields fields})))

        {:ast/type :function}
        (let [method-type (lookup-method-type module type)
              return-type (last method-type)]
          (str
            (if (void? return-type) "lang.function.Consumer" "lang.function.Function")
            (-> method-type (count) (dec))))

        _
        (or
          (get (all-classes module) type)
          (throw (ex-info "Could not find class for type"
                   {:type   type
                    :module (:name module)})))))))

(defn- add-type
  [module type class]
  (swap! (:code-generator/classes module)
    assoc type class))

(defn- push-atom
  [{:keys [atom value]}]
  (case atom
    :integer
    [[:new BigInteger]
     [:dup]
     [:ldc value]
     [:invokespecial BigInteger :init [String VOID]]]

    :string
    [[:ldc value]]

    :boolean
    [[:ldc (case value true 1 false 0)]
     [:invokestatic Boolean "valueOf" [Boolean/TYPE Boolean]]]))

(defn- compare-atoms
  [{:keys [atom]}]
  (case atom
    :integer
    [[:invokevirtual BigInteger "compareTo" [BigInteger Integer/TYPE]]]

    :boolean
    [[:invokevirtual Boolean "compareTo" [Boolean Integer/TYPE]]]

    :string
    [[:invokevirtual String "compareTo" [String Integer/TYPE]]]))

(defn- pattern->instructions
  [module load-body patterns next]
  (let [compile-pattern
        (fn [pattern]
          (match pattern
            {:ast/pattern :atom :atom atom}
            (utils/concatv
              load-body
              (push-atom atom)
              (compare-atoms atom)
              [[:ifne next]])

            {:ast/pattern :variant :variant {:injector injector :value sub-pattern}}
            (let [{:keys [outer inner]} (lookup-injector module injector)]
              (utils/concatv
                load-body
                [[:instanceof outer]
                 [:ifeq next]]
                (when (and (some? sub-pattern) (some? inner)) ; distinguish between true variant and enum
                  (let [sub-register (next-register module inner)]
                    (utils/concatv
                      load-body
                      [[:checkcast outer]
                       [:getfield outer :value inner]
                       [(store inner) sub-register]]
                      (pattern->instructions module
                        [[(load inner) sub-register]]
                        [sub-pattern]
                        next))))))

            {:ast/pattern :record :fields fields}
            (let [outer (lookup-record module (keys fields))]
              (vec
                (mapcat
                  (fn [[field sub-pattern]]
                    (let [inner        (lookup-field module field)
                          sub-register (next-register module inner)]
                      (utils/concatv
                        load-body
                        [[:getfield outer (:name field) inner]
                         [(store inner) sub-register]]
                        (pattern->instructions module
                          [[(load inner) sub-register]]
                          [sub-pattern]
                          next))))
                  fields)))

            {:ast/pattern :symbol :symbol symbol}
            (let [type     (lookup-type module (:type-checker.pattern/type pattern))
                  register (next-register module type)]
              (add-binding module symbol [[(load type) register]])
              (utils/concatv
                load-body
                [[(store type) register]]))

            {:ast/pattern :wildcard}
            []))]
    (->> patterns
      (map compile-pattern)
      (reduce utils/concatv))))

(defn- branch->instructions
  [module load-body {:keys [patterns action]} {:keys [current next success]}]
  (utils/concatv
    [[:mark current]]
    (pattern->instructions module load-body patterns next)
    (->instructions module action)
    [[:goto success]]))

(defn- ->instructions
  [module term]
  (match term
    {:ast/term  :application
     :function  function
     :arguments arguments}
    (let [return-type (lookup-type module (:type-checker.term/type term))
          arguments*  (mapcat (partial ->instructions module) arguments)
          function*   (->instructions module function)
          ct          (count arguments) 
          interface   (str (if (void? return-type)
                           "lang.function.Consumer"
                           "lang.function.Function")
                        ct)
          invoke-insn
          [:invokeinterface
           interface
           (str "apply" ct)
           (utils/concatv
             (repeat ct Object)
             [(if (void? return-type) VOID Object)])]]
      (utils/concatv
        function*
        arguments*
        [invoke-insn] 
        (when (reference? return-type) [[:checkcast return-type]])))

    {:ast/term :match :body body :branches branches}
    (let [body-type         (lookup-type module (:type-checker.term/type body))
          body-register     (next-register module body-type)
          return-type       (lookup-type module (:type-checker.term/type term))
          load-body         [[(load body-type) body-register]]
          [success & marks] (vec (repeatedly (+ 2 (count branches)) #(next-mark module)))
          failure           (last marks)
          mark-seq          (->> marks
                              (partition 2 1)
                              (map #(assoc (zipmap [:current :next] %) :success success)))]
      (utils/concatv
        (->instructions module body)
        [[(store body-type) body-register]]
        (mapcat
          (partial branch->instructions module load-body)
          branches
          mark-seq)
        [[:mark failure]
         [:new Exception] ; TODO: IllegalArgumentException
         [:dup]
         [:invokespecial Exception :init [VOID]]
         [:athrow]
         [:mark success]]
        (when (reference? return-type) [[:checkcast return-type]])))

    {:ast/term :sequence :operations operations}
    (->> operations
      (mapcat (partial ->instructions module))
      (vec))

    {:ast/term :symbol :symbol (symbol :guard jvm/native?)}
    (let [class (->> symbol :in :name (str/join "."))
          field (:name symbol)
          type  (:class (:type-checker.term/type term))]
      [[:getstatic class field type]])

    {:ast/term :symbol :symbol symbol}
    (lookup-binding module symbol)

    {:ast/term :variant :variant {:injector injector :value value}}
    (let [{:keys [outer inner]} (lookup-injector module injector)]
      (utils/concatv
        [[:new outer]
         [:dup]]
        (some->> value (->instructions module))
        [[:invokespecial outer :init (filterv some? [inner VOID])]]))

    {:ast/term :record :fields fields}
    (let [outer         (lookup-record module (keys fields))
          argument-desc (->> fields
                          (keys)
                          (mapv (partial lookup-field module)))]
      (utils/concatv
        [[:new outer]
         [:dup]]
        (mapcat
          (fn [[field value]] (->instructions module value))
          fields)
        [[:invokespecial outer :init (conj argument-desc VOID)]]))

    {:ast/term :extract :record record :field field}
    (let [record-type (lookup-type module (:type-checker.term/type record))
          field-type  (lookup-type module (:type-checker.term/type term))]
      (utils/concatv
        (->instructions module record)
        [[:getfield record-type (:name field) field-type]]))

    {:ast/term :access
     :object   object
     :field    {:ast/term  :application
                :function  function
                :arguments arguments}} ; instance method
    (let [object*    (->instructions module object)
          arguments* (mapcat (partial ->instructions module) arguments)
          invocation [[:invokevirtual
                       (str/join "." (:name (:in (:symbol function))))
                       (:name (:symbol function))
                       (:signature (:type-checker.term/type function))]]]
      (utils/concatv
        object*
        arguments*
        invocation))

    {:ast/term :atom :atom atom}
    (push-atom atom)))

(defn- allocate-registers
  [method]
  (update method :emit
    #(->> %
       (reduce
         (fn [{:keys [allocated next] :as state} instruction]
           (match instruction
             [opcode {:register i :width w}]
             (if-let [register (get allocated i)]
               (update state :instructions conj [opcode register])
               (recur
                 (-> state
                   (update :allocated assoc i next)
                   (update :next + w))
                 instruction))

             _ (update state :instructions conj instruction)))
         {:instructions []
          :allocated    {}
          :next         (->> method
                          :desc
                          (butlast)
                          (map (fn [type] (if-not (wide? type) 1 2)))
                          (reduce +))})
       :instructions)))

(defn- assign-marks
  [method]
  (update method :emit
    #(->> %
       (reduce
         (fn [{:keys [assigned next] :as state} instruction]
           (match instruction
             [opcode {:mark i}]
             (do
               (if-let [mark (get assigned i)]
                 (update state :instructions conj [opcode mark])
                 (recur
                   (-> state
                     (update :assigned assoc i next)
                     (update :next inc))
                   instruction)))

             _ (update state :instructions conj instruction)))
         {:instructions [] :assigned {} :next 0})
       :instructions)))

(defn- make-callsite
  [module {:keys [desc name]}]
  (let [class          (class-name module)
        argument-count (dec (count desc))
        return-type    (last desc)
        arity          (fn [s] (str s argument-count))]
    [[:invokedynamic
      (arity "apply")
      [(arity (if (void? return-type)
                "lang.function.Consumer"
                "lang.function.Function"))]
      [:invokestatic
       LambdaMetafactory
       "metafactory"
       [MethodHandles$Lookup String MethodType MethodType MethodHandle MethodType CallSite]]
      [(insn.util/method-type
         (utils/concatv
           (repeat argument-count Object)
           [(if (void? return-type) VOID Object)]))
       (insn.util/handle :invokestatic class name desc)
       (insn.util/method-type desc)]]]))

(defn- push-arguments
  ([module term]
   (push-arguments module term 0))
  ([module term register]
   (match term
     {:ast/term          :recur
      :reference         reference
      :body              body
      :type-checker.term/type type}
     (let [body-type (lookup-method-type module type)]
       (->> {:name (:name reference)
             :desc body-type}
         (make-callsite module)
         (add-binding module reference))
       (recur module body register))

     {:ast/term          :lambda
      :argument          argument
      :body              body
      :type-checker.term/type type}
     (let [argument-type (->> type
                           (specialize-type)
                           :domain
                           (lookup-type module))]
       (add-binding module argument [[(load argument-type) register]])
       (recur module
         body
         (cond-> (inc register) (wide? argument-type) inc)))

     _ term)))

(defn- promote-lambdas
  [module parent term]
  (let [methods (atom [])
        term*
        (walk/prewalk
          (fn [node]
            (match node
              {:ast/term :lambda} ; TODO: captured variables
              (let [type           (lookup-method-type module (:type-checker.term/type node))
                    argument-types (butlast type)
                    return-type    (last type)
                    lambda-method
                    {:flags #{:private :static}
                     :name  (format "%s$fn%d" parent (count @methods))
                     :desc  type
                     :emit  (utils/concatv
                              (->> node
                                (push-arguments module)
                                (->instructions module))
                              [[(return return-type)]])}
                    reference      {:reference :variable :name (:name lambda-method)}]
                (swap! methods conj lambda-method)
                (->> lambda-method
                  (make-callsite module)
                  (add-binding module reference))
                (-> node
                  (select-keys [:type-checker.term/type])
                  (merge {:ast/term :symbol
                          :symbol   reference})))

              _ node))
          term)]
    {:methods @methods :term term*}))

(defn- ->field
  [module {:keys [name body]}]
  (let [class                  (class-name module)
        type                   (->> body :type-checker.term/type (lookup-type module))
        name*                  (munge (:name name))
        _ (add-binding module
            name
            [[:getstatic class name* type]])
        {:keys [methods term]} (promote-lambdas module name* body)
        field
        (-> term
          (match
            {:ast/term :atom :atom (atom :guard #(-> % :atom (not= :integer)))}
            {:value (:value atom)}

            _
            (do (swap! (get-in module [:code-generator/bytecode class :code-generator/static-initializer-instructions])
                  utils/concatv
                  (conj
                    (->instructions module term)
                    [:putstatic class name* type]))
                {}))
          (merge
            {:flags #{:public :static :final}
             :name  name*
             :type  type}))]
    {class {:fields  [field]
            :methods methods}}))

(defn- ->methods
  [module definition]
  (let [methods
        (match definition
          {:ast/definition :constant
           :name           name
           :body           body}
          (let [{:keys [term methods]}
                (->> body
                  (push-arguments module)
                  (promote-lambdas module (:name name)))
                type (lookup-method-type module (:type-checker.term/type body))
                stub {:name (munge (:name name))
                      :desc type}]
            (->> stub
              (make-callsite module)
              (add-binding module name))
            (->> {:flags #{:public :static}
                  :emit   (utils/concatv
                            (->instructions module term)
                            [[(return (last type))]]) }
              (merge stub)
              (conj methods))))]
    {(class-name module) {:methods methods}}))

(declare type->classes)

(defn- injector->classes
  [module supertype [injector type]]
  (let [name           (subclass module supertype injector)
        anonymous-inner
        (when (contains? #{:record} (:ast/type type))
          (->> {:name {:reference :type
                       :name      (format "%s$%s$inner" (:name supertype) (:name injector))}
                :body type}
            (type->classes module)
            (first)
            (val)))
        jvm-type       (or (:name anonymous-inner) (some->> type (lookup-type module)))
        injector-class (merge
                         {:flags   #{:public :final}
                          :name    name
                          :version bytecode-version
                          :super   (:class supertype)
                          :methods [{:flags #{:public}
                                     :name  :init
                                     :desc  (filterv some? [jvm-type VOID])
                                     :emit  (utils/concatv
                                              [[:aload 0]
                                               [:invokespecial :super :init [VOID]]]
                                              (when type
                                                [[:aload 0]
                                                 [(load jvm-type) 1]
                                                 [:putfield :this :value jvm-type]])
                                              [[:return]])}]}
                         (when type
                           {:fields [{:flags #{:public :final}
                                      :name  :value
                                      :type  jvm-type}]}))]
    (add-injector module injector name jvm-type)
    (filterv some? [injector-class anonymous-inner])))

(defn- type->classes
  [module {:keys [name body]}]
  (let [class (subclass module name)]
    (add-type module (assoc name :in (:name module)) class)
    (->>
      (match (specialize-type body)
        {:ast/type :variant :injectors injectors}
        (let [super {:flags   #{:public :abstract}
                     :name    class
                     :version bytecode-version
                     :methods [{:flags #{:protected}
                                :name  :init
                                :desc  [VOID]
                                :emit  [[:aload 0]
                                        [:invokespecial :super :init [VOID]]
                                        [:return]]}]}]
          (add-variant module (keys injectors) class)
          (conj
            (mapcat (partial injector->classes module (assoc name :class class)) injectors)
            super))

        {:ast/type :record :fields fields}
        (let [argument-desc (mapv (partial lookup-type module) (vals fields))
              desc          (conj argument-desc VOID)]
          (add-record module (keys fields) class)
          [{:flags   #{:public}
            :name    class
            :version bytecode-version
            :methods [{:flags #{:public}
                       :name  :init
                       :desc  desc
                       :emit  (utils/concatv
                                [[:aload 0]
                                 [:invokespecial :super :init [VOID]]]
                                (mapcat (fn [i [field type]]
                                          (let [jvm-type (lookup-type module type)]
                                            [[:aload 0]
                                             [(load jvm-type) (inc i)]
                                             [:putfield :this (:name field) jvm-type]]))
                                  (range)
                                  fields)
                                [[:return]])}]
            :fields  (mapv (fn [[field type]]
                             (let [jvm-type (lookup-type module type)] 
                               (add-field module field jvm-type)
                               {:flags #{:public :final}
                                :name  (:name field)
                                :type  jvm-type}))
                       fields)}]))
      (map (fn [class] [(:name class) class]))
      (into {}))))

(defn- combine
  [module bytecode]
  (letfn [(finalize-method [method]
            (-> method
              (allocate-registers)
              (assign-marks)))]
    (let [bytecode
          (->> bytecode
            (map (fn [[class body]]
                   [class
                    (-> body
                      (update :methods (partial mapv finalize-method)))]))
            (into (empty bytecode)))]
      (update module :code-generator/bytecode
        #(merge-with
           (fn [class-1 class-2]
             (merge-with utils/concatv class-1 class-2))
           % bytecode)))))

(defn- definition->bytecode
  [module definition]
  (let [class         (class-name module)
        reverse-merge (fn [x y] (merge y x))
        name          (:name definition)]
    (match definition
      {:ast/definition :type :body body}
      (combine module (type->classes module definition))

      {:ast/definition :constant :body {:ast/term :recur}}
      (combine module (->methods module definition))

      {:ast/definition :constant}
      (combine module (->field module definition))

      {:ast/definition :macro}
      module)))

(defn update-tables
  [module]
  (-> module
    (update :classes merge @(:code-generator/classes module))
    (update :instructions merge @(:code-generator/bindings module))))

(defn- add-static-initializer
  [module]
  (let [name (class-name module)]
    (update-in module [:code-generator/bytecode name]
      (fn [class]
        (let [instructions @(:code-generator/static-initializer-instructions class)
              method
              {:flags #{:public :static}
               :name  :clinit
               :desc  [VOID]
               :emit  (utils/concatv
                        [[:new name]
                         [:dup]
                         [:invokespecial name :init [VOID]]]
                        instructions
                        [[:return]])}]
          (-> class
            (dissoc :code-generator/static-initializer-instructions)
            (update :methods conj method)))))))

(defn- resort
  [module]
  (let [class (class-name module)]
    (update module :code-generator/bytecode
      (fn [bytecode]
        (-> bytecode
          (dissoc class)
          (assoc class (get bytecode class)))))))

(defn- module->bytecode
  [module]
  (let [name    (class-name module)
        module* (-> module
                  (assoc-in [:code-generator/bytecode name]
                    {:name    name
                     :version bytecode-version
                     :flags   #{:public :final :super}
                     :code-generator/static-initializer-instructions
                     (atom [])})
                  (merge #:code-generator{:next-register (atom 0)
                                          :next-mark     (atom 0)
                                          :bindings      (atom {})
                                          :classes       (atom {})}))]
    (->
      (reduce
        (comp update-tables definition->bytecode)
        module*
        (:definitions module))
      (add-static-initializer)
      (resort))))

(extend-protocol insn.core/Loader ; FIXME: reflection warning
  ClassLoader
  (load-type [classloader t]
    (let [get-declared-method (.getDeclaredMethod ClassLoader
                                "defineClass"
                                (into-array [String (Class/forName "[B") Integer/TYPE Integer/TYPE]))]
      (try
        (.setAccessible get-declared-method true)
        (.invoke get-declared-method classloader
          (into-array Object [(:name t) (:bytes t) (int 0) (int (count (:bytes t)))]))
        (catch Exception e
          nil)
        (finally
          (.setAccessible get-declared-method false))))))

(defn run
  [module]
  (let [module      (module->bytecode module)
        classloader (ClassLoader/getSystemClassLoader)]
    (->> module
      :code-generator/bytecode
      (vals)
      (run!
        #(let [class (insn/visit %)]
           (insn/define classloader class)
           (insn/write class "out/"))))
    module))

(comment

  (-> "examples/option.lang"
    (lang.compiler/run #{:parser :name-resolution :dependency-analyzer :type-checker :code-generator})
    :code-generator/bytecode)

  (-> "std/lang/option.lang"
    (lang.compiler/run #{:parser :name-resolution :dependency-analyzer :type-checker :code-generator})
    :code-generator/bytecode)

  (throw (ex-info "foo" {}))

  (com.gfredericks.debug-repl/unbreak!!)

  (com.gfredericks.debug-repl/unbreak!)

  )
