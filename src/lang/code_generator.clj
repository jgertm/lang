(ns lang.code-generator
  (:refer-clojure :exclude [load])
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [insn.core :as insn]
            insn.util
            [lang.utils :as utils :refer [undefined]])
  (:import [java.lang.invoke CallSite LambdaMetafactory MethodHandle MethodHandles$Lookup MethodType]))

(declare ->instructions)

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
  (get {:int :iload} type :aload))

(defn- store
  [type]
  (get {:int :istore} type :astore))

(defn- return
  [type]
  (get {:void :return
        :int  :ireturn} type :areturn))

(defn- primitive?
  [type]
  (contains? #{:byte :short :int :long :char :float :double :void} type))

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

(defn- lookup-type
  [module type]
  (let [primitives
        (->> {:string  String ; TODO: complete
              :unit    :void
              :integer :int
              :boolean 'boolean
              :object  Object}
          (map (fn [[k v]] [{:ast/type :primitive :primitive k} v]))
          (into {}))]
    (-> type
      (specialize-type)
      (match
        {:ast/type :primitive}
        (get primitives type)

        {:ast/type :existential-variable} ; TODO: FIXME?
        Object

        {:ast/type :function :domain domain :return return}
        (let [domain* (lookup-type module domain)
              return* (lookup-type module return)]
          (if (vector? return*) ; FIXME: returns array
            (into [domain*] return*)
            (conj [domain*] return*)))

        _
        (or
          (get @(:code-generator/types module) type)
          (get @(:code-generator/types module) (:type-checker/instance-of type)))))))

(defn- add-type
  [module type class]
  (swap! (:code-generator/types module)
    assoc type class))

(defn- lookup-binding
  [module symbol]
  (get @(:code-generator/bindings module) symbol))

(defn- add-binding
  [module symbol instructions]
  (swap! (:code-generator/bindings module)
    assoc symbol instructions))

(defn- lookup-function
  [module symbol]
  (or
    (get @(:code-generator/functions module) symbol)
    (get @(:code-generator/bindings module) symbol)))

(defn- add-function
  [module symbol fn]
  (swap! (:code-generator/functions module)
    assoc symbol fn))

(defn- lookup-variant
  [module injector]
  (get @(:code-generator/variants module) injector))

(defn- add-variant
  [module injector {:keys [class super type] :as variant}]
  (swap! (:code-generator/variants module)
    assoc injector variant))

(defn- pattern->instructions
  [module body-info pattern next]
  (let [load-body [(load (:type body-info)) (:register body-info)]]
    (match pattern
      {:ast/pattern :variant :variant {:injector injector :value value}}
      (let [{:keys [class type]} (lookup-variant module injector)]
        (utils/concatv
          [load-body
           [:instanceof class]
           [:ifeq next]]
          (when (and (some? value) (some? type)) ; distinguish between true variant and enum
            (let [sub-register (next-register module type)]
              (utils/concatv
                [load-body
                 [:checkcast class]
                 [:getfield class :value type]
                 [(store type) sub-register]]
                (pattern->instructions module {:register sub-register :type type} value next))))))

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
          arguments*
          (->> arguments
            (mapcat (partial ->instructions module)))
          function* (lookup-function module (:symbol function))]
      (utils/concatv
        (function* arguments*)
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
         [:invokespecial Exception :init [:void]]
         [:athrow]
         [:mark success]]
        (when (reference? return-type) [[:checkcast return-type]])))

    {:ast/term :symbol :symbol symbol}
    (lookup-binding module symbol)

    {:ast/term :variant :variant {:injector injector :value value}}
    (let [{:keys [class type]} (lookup-variant module injector)]
      (utils/concatv
        [[:new class]
         [:dup]]
        (some->> value (->instructions module))
        [[:invokespecial class :init (filterv some? [type :void])]]))

    {:ast/term :atom :atom {:value value}}
    [[:ldc value]]))

(defn- ->field
  [module {:keys [name body]}]
  (let [class (class-name module)
        type  (->> body :type-checker/type (lookup-type module))
        field
        (-> body
          (match
            {:ast/term :atom :atom atom}
            {:value (:value atom)}

            {:ast/term :variant}
            (do (swap! (get-in module [:bytecode class :code-generator/static-initializer-instructions])
                  utils/concatv (conj (->instructions module body) [:putstatic class (:name name) type]))
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
     {:ast/term :lambda :argument argument :body body}
     (let [argument-type (lookup-type module (:domain (:type-checker/type module)))]
       (add-binding module argument [[(load argument-type) register]])
       (recur
         module
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
              (let [type           (lookup-type module (:type-checker/type node))
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
                (add-binding module reference
                  (match (:desc lambda-method)
                    [_ _]
                    (fn [args]
                      (utils/concatv
                        [[:invokedynamic
                          "apply1"
                          ["lang.function.Function1"]
                          [:invokestatic
                           LambdaMetafactory
                           "metafactory"
                           [MethodHandles$Lookup String MethodType MethodType MethodHandle MethodType CallSite]]
                          [(insn.util/method-type [Object Object])
                           (insn.util/handle :invokestatic (class-name module) (:name lambda-method) (:desc lambda-method))
                           (insn.util/method-type (:desc lambda-method))]]]
                        args
                        [[:invokeinterface "lang.function.Function1" "apply1" [Object Object]]]))

                    [_ _ _]
                    (fn [args]
                      (utils/concatv
                        [[:invokedynamic
                          "apply2"
                          ["lang.function.Function2"]
                          [:invokestatic
                           LambdaMetafactory
                           ""
                           [MethodHandles$Lookup String MethodType MethodType MethodHandle MethodType CallSite]]
                          [(insn.util/method-type [Object Object Object])
                           (insn.util/handle :invokestatic (class-name module) (:name lambda-method) (:desc lambda-method))
                           (insn.util/method-type (:desc lambda-method))]]]
                        args
                        [[:invokeinterface "lang.function.Function2" "apply2" [Object Object Object]]]))))
                (merge
                  (select-keys node [:type-checker/type])
                  {:ast/term :symbol
                   :symbol   reference}))

              _ node))
          term)]
    {:methods @methods :term term*}))

(defn- ->methods
  [module definition]
  (->>
    (match definition
      {:ast/definition :constant
       :name           {:reference :variable :name "main"}
       :body           {:ast/term :lambda :body body}}
      (let [{:keys [term methods]}
            (promote-lambdas module "main" body)
            method {:flags #{:public :static}
                    :name  "main"
                    :desc  [[String] :void]
                    :emit  (utils/concatv
                             (->instructions module term)
                             [[(return :void)]])}]
        (conj methods method))

      {:ast/definition :constant
       :name           name
       :body           body}
      (let [{:keys [term lambdas]}
            (promote-lambdas module (:name name) (push-arguments module body))
            type   (lookup-type module (:type-checker/type body))
            method {:flags #{:public :static}
                    :name  (:name name)
                    :desc  type
                    :emit  (utils/concatv
                             (->instructions module term)
                             [[(return (last type))]]) }]
        (add-binding module name
          (match type
            [_ _] 
            (fn [args]
              (utils/concatv
                [[:invokedynamic
                  "apply1"
                  ["lang.function.Function1"]
                  [:invokestatic
                   LambdaMetafactory
                   "metafactory"
                   [MethodHandles$Lookup String MethodType MethodType MethodHandle MethodType CallSite]]
                  [(insn.util/method-type [Object Object])
                   (insn.util/handle :invokestatic (class-name module) (:name method) (:desc method))
                   (insn.util/method-type (:desc method))]]]
                args
                [[:invokeinterface "lang.function.Function1" "apply1" [Object Object]]]))

            [_ _ _] 
            (fn [args]
              (utils/concatv
                [[:invokedynamic
                  "apply2"
                  ["lang.function.Function2"]
                  [:invokestatic
                   LambdaMetafactory
                   "metafactory"
                   [MethodHandles$Lookup String MethodType MethodType MethodHandle MethodType CallSite]]
                  [(insn.util/method-type [Object Object Object])
                   (insn.util/handle :invokestatic (class-name module) (:name method) (:desc method))
                   (insn.util/method-type (:desc method))]]]
                args
                [[:invokeinterface "lang.function.Function2" "apply2" [Object Object Object]]]))))
        (conj lambdas method)))
    (mapv
      #(-> %
         (allocate-registers)
         (assign-marks)))))

(defn- register-native
  [module definition]
  (add-function module
    (:name definition)
    (match definition
      {:body {:function method :arguments [object]} :type type}
      (fn [args]
        (utils/concatv
          [[:getstatic
            (->> object :symbol :in :name (str/join "."))
            (-> object :symbol :name)]]
          args
          [[:invokevirtual
            (->> method :symbol :in :name (str/join "."))
            (-> method :symbol :name)
            (lookup-type module type)]])))))

(defn- variant->class
  [module super [injector type]]
  (let [type (lookup-type module type)
        name (format "%s$%s" super (:name injector))]
    (add-variant module injector {:class name :super super :type type})
    (->
      (if (nil? type) ; distinguish between true variant and enum
        {:methods [{:flags #{:public}
                    :name  :init
                    :desc  [:void]
                    :emit  [[:aload 0]
                            [:invokespecial :super :init [:void]]
                            [:return]]}]}
        {:methods [{:flags #{:public}
                    :name  :init
                    :desc  [type :void]
                    :emit  [[:aload 0]
                            [:invokespecial :super :init [:void]]
                            [:aload 0]
                            [(load type) 1]
                            [:putfield :this :value type]
                            [:return]]}]
         :fields  [{:flags #{:public :final}
                    :name  :value
                    :type  type}]})
      (merge {:flags #{:public :final}
              :name  name
              :version 11
              :super super}))))

(defn- type->classes
  [module {:keys [name body]}]
  (let [class (subclass module name)]
    (add-type module body class)
    (match (specialize-type body)
      {:ast/type :variant :variants variants}
      (let [super {:flags   #{:public :abstract}
                   :name    class
                   :version 11
                   :methods [{:flags #{:protected}
                              :name  :init
                              :desc  [:void]
                              :emit  [[:aload 0]
                                      [:invokespecial :super :init [:void]]
                                      [:return]]}]}]
        (->> super
          (conj (map (partial variant->class module (:name super)) variants))
          (map (fn [class] [(:name class) class]))
          (into {}))))))

(defn- definition->bytecode
  [module definition]
  (let [class (class-name module)]
    (match definition
      {:ast/definition :type}
      (->> definition
        (type->classes module)
        (update module :bytecode merge))

      {:ast/definition :constant :body {:ast/term :lambda}}
      (->> definition
        (->methods module)
        (update-in module [:bytecode class :methods] utils/concatv))

      {:ast/definition :constant}
      (->> definition
        (->field module)
        (update-in module [:bytecode class :fields] utils/conjv))

      {:ast/definition :native}
      (do (register-native module definition )
          module))))

(defn- add-static-initializer
  [module]
  (let [name (class-name module)]
    (update-in module [:bytecode name]
      (fn [class]
        (let [instructions @(:code-generator/static-initializer-instructions class)
              method
              (-> {:flags #{:public :static}
                   :name  :clinit
                   :desc  [:void]
                   :emit  (utils/concatv
                            [[:new name]
                             [:dup]
                             [:invokespecial name :init [:void]]]
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
                  (assoc-in [:bytecode name]
                    {:name name
                     :version 11
                     :flags #{:public :final :super}
                     :code-generator/static-initializer-instructions
                     (atom [])})
                  (merge #:code-generator{:next-register (atom 0)
                                          :next-mark     (atom 0)
                                          :bindings      (atom {})
                                          :functions     (atom {})
                                          :types         (atom {})
                                          :variants      (atom {})}))]
    (->
      (reduce definition->bytecode
        module*
        (:definitions module))
      (add-static-initializer))))

(defn run
  [module]
  (let [module (module->bytecode module)]
    (->> module
      :bytecode
      (vals)
      (vec)
      (run!
        #(-> %
           (insn/visit)
           (insn/write "out/"))))
    module))

(comment

  (-> "examples/hello.lang"
    (lang.compiler/run)
    :bytecode)

  )
