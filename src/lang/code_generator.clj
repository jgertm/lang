(ns lang.code-generator
  (:refer-clojure :exclude [load])
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [insn.core :as insn]
            insn.util
            [lang.jvm :as jvm]
            [lang.module :as module]
            [lang.type :as type]
            [lang.utils :as utils :refer [undefined]])
  (:import [java.lang.invoke CallSite LambdaMetafactory MethodHandle MethodHandles$Lookup MethodType]))

(declare lookup-type)
(declare ->instructions)

(def ^:private bytecode-version 11)

(def ^:private VOID Void/TYPE)
(def ^:private INT Integer/TYPE)
(def ^:private BOOL Boolean/TYPE)

(defn- void?
  [type]
  (= type VOID))

(defn- class-name
  [module]
  (str/join "." (:name (:name module))))

(defn- subclass
  [module class]
  (format "%s.%s"
    (class-name module)
    (:name class)))

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
    {:ast/type :forall :variable variable :body body}
    (->> body
      (walk/prewalk-replace {variable {:ast/type :primitive :primitive :object}})
      (recur))

    _ type))

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

(defn- all-types
  [module]
  (merge
    (module/all-types module)
    @(:code-generator/types module)))

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
        (get primitives type)

        {:ast/type :vector :inner inner}
        [(lookup-type module inner)]

        {:ast/type (:or :existential-variable :universal-variable)} ; TODO: FIXME?
        Object

        {:ast/type :function}
        (let [method-type (lookup-method-type module type)
              return-type (last method-type)]
          (case (->> method-type (count) (dec))
            1 (if (void? return-type) "lang.function.Consumer1" "lang.function.Function1")
            2 (if (void? return-type) "lang.function.Consumer2" "lang.function.Function2")))

        _
        (let [types (all-types module)]
          (:class
           (or
             (get types type)
             (get types (:type-checker/instance-of type))
             (throw (ex-info "Could not find class for type"
                      {:type type
                       :module (:name module)})))))))))

(defn- add-type
  [module type class]
  (swap! (:code-generator/types module)
    assoc type {:class class}))

(defn- local-bindings
  [module]
  (->> module
    :code-generator/bindings
    (deref)
    (map (fn [[k v]] [k {:instructions v}]))
    (into {})))

(defn- all-bindings
  [module]
  (merge
    (module/all-bindings module)
    (local-bindings module)))

(defn- lookup-binding
  [module symbol]
  (or
    (:instructions (get (all-bindings module) symbol))
    (throw (ex-info "Could not find instructions for binding"
             {:symbol symbol
              :module (:name module)}))))

(defn- add-binding
  [module symbol instructions]
  (swap! (:code-generator/bindings module)
    assoc (select-keys symbol [:reference :name]) instructions))

(defn- all-injectors
  [module]
  (merge
    (module/all-injectors module)
    @(:code-generator/injectors module)))

(defn- lookup-injector
  [module injector]
  (get (all-injectors module) injector))

(defn- add-injector
  [module injector variant]
  (swap! (:code-generator/injectors module)
    assoc injector variant))

(defn- push-atom
  [atom]
  [(if (contains? #{:long :double} (:atom atom))
     :ldc2
     :ldc)
   (match atom
     {:atom (:or :integer :string) :value value}
     value

     {:atom :boolean :value value}
     (case value
       true  1
       false 0))])

(defn- compare-atoms
  [atom]
  (case (:atom atom)
    :integer
    [:invokestatic Integer "compare" [INT INT INT]] 

    :boolean
    [:invokestatic Boolean "compare" [BOOL BOOL INT]]

    :string
    [:invokevirtual String "compareTo" [String String INT]]))

(defn- pattern->instructions
  [module body-info pattern next]
  (let [load-body [(load (:type body-info)) (:register body-info)]]
    (match pattern
      {:ast/pattern :atom :atom atom}
      [load-body
       (push-atom atom)
       (compare-atoms atom)
       [:ifeq next]]

      {:ast/pattern :variant :variant {:injector injector :value value}}
      (let [{:class/keys [outer inner]} (lookup-injector module injector)]
        (utils/concatv
          [load-body
           [:instanceof outer]
           [:ifeq next]]
          (when (and (some? value) (some? inner)) ; distinguish between true variant and enum
            (let [sub-register (next-register module inner)]
              (utils/concatv
                [load-body
                 [:checkcast outer]
                 [:getfield outer :value inner]
                 [(store inner) sub-register]]
                (pattern->instructions module {:register sub-register :type inner} value next))))))

      {:ast/pattern :symbol :symbol symbol}
      (let [type     (:type body-info)
            register (next-register module type)]
        (add-binding module symbol [[(load type) register]])
        [load-body
         [(store type) register]])

      {:ast/pattern :wildcard}
      [])))

(defn- branch->instructions
  [module body-info {:keys [pattern action]} {:keys [current next success]}]
  (utils/concatv
    [[:mark current]]
    (pattern->instructions module body-info pattern next)
    (->instructions module action)
    [[:goto success]]))

(defn- ->instructions
  [module term]
  (match term
    {:ast/term  :application
     :function  function
     :arguments arguments}
    (let [return-type (lookup-type module (:type-checker/type term))
          arguments*  (mapcat (partial ->instructions module) arguments)
          function*   (lookup-binding module (:symbol function))
          invoke-insn
          (case (+ (dec (count function*)) (count arguments))
            1 [:invokeinterface
               (if (void? return-type) "lang.function.Consumer1" "lang.function.Function1")
               "apply1"
               [Object (if (void? return-type) VOID Object)]]
            2 [:invokeinterface
               (if (void? return-type) "lang.function.Consumer2" "lang.function.Function2")
               "apply2"
               [Object Object (if (void? return-type) VOID Object)]])]
      (utils/concatv
        function*
        arguments*
        [invoke-insn] 
        (when (reference? return-type) [[:checkcast return-type]])))

    {:ast/term :match :body body :branches branches}
    (let [body-type         (lookup-type module (:type-checker/type body))
          return-type       (lookup-type module (:type-checker/type term))
          body-info         {:register (next-register module body-type) :type body-type}
          [success & marks] (repeatedly (+ 2 (count branches)) #(next-mark module))
          failure           (last marks)
          mark-seq          (->> marks
                              (partition 2 1)
                              (map #(assoc (zipmap [:current :next] %) :success success)))]
      (utils/concatv
        (->instructions module body)
        [[(store body-type) (:register body-info)]]
        (mapcat
          (partial branch->instructions module body-info)
          branches
          mark-seq)
        [[:mark failure]
         [:new Exception] ; TODO: IllegalArgumentException
         [:dup]
         [:invokespecial Exception :init [VOID]]
         [:athrow]
         [:mark success]]
        (when (reference? return-type) [[:checkcast return-type]])))

    {:ast/term :symbol :symbol (symbol :guard jvm/native?)}
    (let [class (->> symbol :in :name (str/join "."))
          field (:name symbol)
          type  (:class (:type-checker/type term))]
      [[:getstatic class field type]])

    {:ast/term :symbol :symbol symbol}
    (lookup-binding module symbol)

    {:ast/term :variant :variant {:injector injector :value value}}
    (let [{:class/keys [outer inner]} (lookup-injector module injector)]
      (utils/concatv
        [[:new outer]
         [:dup]]
        (some->> value (->instructions module))
        [[:invokespecial outer :init (filterv some? [inner VOID])]]))

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
                       (:signature (:type-checker/type function))]]]
      (utils/concatv
        object*
        arguments*
        invocation))

    {:ast/term :atom :atom atom}
    [(push-atom atom)]))

(defn- ->field
  [module {:keys [name body]}]
  (let [class (class-name module)
        type  (->> body :type-checker/type (lookup-type module))
        field
        (-> body
          (match
            {:ast/term :atom :atom atom}
            {:value (:value atom)}

            _
            (do (swap! (get-in module [:code-generator/bytecode class :code-generator/static-initializer-instructions])
                  utils/concatv
                  (conj
                    (->instructions module body)
                    [:putstatic class (:name name) type]))
                {}))
          (merge
            {:flags #{:public :static :final}
             :name  (:name name)
             :type  type}))]
    (add-binding module
      name
      [[:getstatic class (:name field) (:type field)]])
    field))

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

(defn- push-arguments
  ([module term]
   (push-arguments module term 0))
  ([module term register]
   (match term
     {:ast/term          :lambda
      :argument          argument
      :body              body
      :type-checker/type type}
     (let [argument-type (->> type
                           (specialize-type)
                           :domain
                           (lookup-type module))]
       (add-binding module argument [[(load argument-type) register]])
       (recur
         module
         body
         (cond-> (inc register) (wide? argument-type) inc)))

     _ term)))

(defn- make-callsite
  [module {:keys [desc name]}]
  (let [class (class-name module)]
    (match desc
      [_ return-type]
      [[:invokedynamic
        "apply1"
        [(if (void? return-type)
           "lang.function.Consumer1"
           "lang.function.Function1")]
        [:invokestatic
         LambdaMetafactory
         "metafactory"
         [MethodHandles$Lookup String MethodType MethodType MethodHandle MethodType CallSite]]
        [(insn.util/method-type [Object (if (void? return-type) VOID Object)])
         (insn.util/handle :invokestatic class name desc)
         (insn.util/method-type desc)]]]

      [_ _ return-type]
      [[:invokedynamic
        "apply2"
        [(if (void? return-type)
           "lang.function.Consumer2"
           "lang.function.Function2")]
        [:invokestatic
         LambdaMetafactory
         "metafactory"
         [MethodHandles$Lookup String MethodType MethodType MethodHandle MethodType CallSite]]
        [(insn.util/method-type [Object Object (if (void? return-type) VOID Object)])
         (insn.util/handle :invokestatic class name desc)
         (insn.util/method-type desc)]]])))

(defn- promote-lambdas
  [module parent term]
  (let [methods (atom [])
        term*
        (walk/prewalk
          (fn [node]
            (match node
              {:ast/term :lambda} ; TODO: captured variables
              (let [type           (lookup-method-type module (:type-checker/type node))
                    argument-types (butlast type)
                    return-type    (last type)
                    lambda-method
                    {:flags #{:private :static}
                     :name  (format "%s$fn%d" parent (count @methods))
                     :desc  type
                     :emit  (utils/concatv
                              (->> node
                                (push-arguments module)
                                (->instructions module)) ; TODO: nested lambdas
                              [[(return return-type)]])}
                    reference {:reference :variable :name (:name lambda-method)}]
                (swap! methods conj lambda-method)
                (->> lambda-method
                  (make-callsite module)
                  (add-binding module reference))
                (-> node
                  (select-keys [:type-checker/type])
                  (merge {:ast/term :symbol
                          :symbol   reference})))

              _ node))
          term)]
    {:methods @methods :term term*}))

(defn- ->methods
  [module definition]
  (->>
    (match definition
      {:ast/definition :constant
       :name           name
       :body           body}
      (let [{:keys [term methods]}
            (->> body
              (push-arguments module)
              (promote-lambdas module (:name name)))
            type   (lookup-method-type module (:type-checker/type body))
            method {:flags #{:public :static}
                    :name  (:name name)
                    :desc  type
                    :emit  (utils/concatv
                             (->instructions module term)
                             [[(return (last type))]]) }]
        (->> method
          (make-callsite module)
          (add-binding module name))
        (conj methods method)))
    (mapv
      #(-> %
         (allocate-registers)
         (assign-marks)))))

(defn- injector->class
  [module super [injector type]]
  (let [type (some->> type (lookup-type module))
        name (format "%s$%s" super (:name injector))]
    (add-injector module injector #:class{:outer name :super super :inner type})
    (->
      (if (nil? type) ; distinguish between true variant and enum
        {:methods [{:flags #{:public}
                    :name  :init
                    :desc  [VOID]
                    :emit  [[:aload 0]
                            [:invokespecial :super :init [VOID]]
                            [:return]]}]}
        {:methods [{:flags #{:public}
                    :name  :init
                    :desc  [type VOID]
                    :emit  [[:aload 0]
                            [:invokespecial :super :init [VOID]]
                            [:aload 0]
                            [(load type) 1]
                            [:putfield :this :value type]
                            [:return]]}]
         :fields  [{:flags #{:public :final}
                    :name  :value
                    :type  type}]})
      (merge {:flags #{:public :final}
              :name  name
              :version bytecode-version
              :super super}))))

(defn- type->classes
  [module {:keys [name body]}]
  (let [class (subclass module name)]
    (add-type module body class)
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
        (->> super
          (conj (map (partial injector->class module (:name super)) injectors))
          (map (fn [class] [(:name class) class]))
          (into {}))))))

(defn- definition->bytecode
  [module definition]
  (let [class         (class-name module)
        reverse-merge (fn [x y] (merge y x))
        name          (:name definition)]
    (match definition
      {:ast/definition :type :body body}
      (let [classes (type->classes module definition)]
        (-> module
          (update :code-generator/bytecode reverse-merge classes)
          (assoc-in [:types name :class] (lookup-type module body))
          (update-in [:types name :injectors]
            (partial merge-with merge)
            (->> body
              (type/injectors)
              (keys)
              (map (fn [injector] [injector (lookup-injector module injector)]))
              (into {})))))

      {:ast/definition :constant :body {:ast/term :lambda}}
      (let [methods (->methods module definition)]
        (-> module
          (update-in [:code-generator/bytecode class :methods] utils/concatv methods)
          (assoc-in [:values name :instructions] (lookup-binding module name))))

      {:ast/definition :constant}
      (let [field (->field module definition)]
        (-> module
          (update-in [:code-generator/bytecode class :fields] utils/conjv field)
          (assoc-in [:values name :instructions] (lookup-binding module name))))

      {:ast/definition :macro}
      module)))

(defn- add-static-initializer
  [module]
  (let [name (class-name module)]
    (update-in module [:code-generator/bytecode name]
      (fn [class]
        (let [instructions @(:code-generator/static-initializer-instructions class)
              method
              (-> {:flags #{:public :static}
                   :name  :clinit
                   :desc  [VOID]
                   :emit  (utils/concatv
                            [[:new name]
                             [:dup]
                             [:invokespecial name :init [VOID]]]
                            instructions
                            [[:return]])}
                (allocate-registers)
                (assign-marks))]
          (-> class
            (dissoc :code-generator/static-initializer-instructions)
            (update :methods conj method)))))))

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
                                          :types         (atom {})
                                          :injectors     (atom {})}))]
    (->
      (reduce definition->bytecode
        module*
        (:definitions module))
      (add-static-initializer))))

(extend-protocol insn.core/Loader ; FIXME: reflection warning
  ClassLoader
  (load-type [classloader t]
    (let [get-declared-method (.getDeclaredMethod ClassLoader "defineClass" (into-array [String (Class/forName "[B") Integer/TYPE Integer/TYPE]))]
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
    (lang.compiler/run #{:parser :dependency-analyzer :type-checker :code-generator})
    :code-generator/bytecode
    )

  (-> "std/lang/option.lang"
    (lang.compiler/run #{:parser :dependency-analyzer :type-checker :code-generator})
    :types
    )

  )
