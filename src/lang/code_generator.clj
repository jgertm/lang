(ns lang.code-generator
  (:refer-clojure :exclude [load])
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [insn.core :as insn]
            [lang.utils :as utils]))

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

(defn- lookup-type
  [module type]
  (let [primitives
        (->> {:string  String ; TODO: complete
              :unit    :void
              :integer :int
              :boolean 'boolean}
          (map (fn [[k v]] [{:ast/type :primitive :primitive k} v]))
          (into {}))]
    (match type
      {:ast/type :primitive}
      (get primitives type)

      {:ast/type :function :domain domain :return return}
      (let [domain* (lookup-type module domain)
            return* (lookup-type module return)]
        (if (vector? return*)
          (into [domain*] return*)
          (conj [domain*] return*)))

      _
      (get @(:code-generator/types module) type))))

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
  (get @(:code-generator/functions module) symbol))

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
      {:ast/pattern :variant :variant [injector sub-pattern]}
      (let [{:keys [class type]} (lookup-variant module injector)
            sub-register         (next-register module type)]
        (utils/concatv
          [load-body
           [:instanceof class]
           [:ifeq next]
           load-body
           [:checkcast class]
           [:getfield class :value type]
           [(store type) sub-register]]
          (pattern->instructions module {:register sub-register :type type} sub-pattern next)))

      {:ast/pattern :symbol :symbol symbol}
      (let [type     (:type body-info)
            register (next-register module type)]
        (add-binding module symbol [[(load type) register]])
        [load-body
         [(store type) register]])

      {:ast/pattern :wildcard}
      [])))

(partition 2 1 (range 5))

(defn- branch->instructions
  [module body-info {:keys [pattern action]} [current next]]
  (utils/concatv
    [[:mark current]]
    (pattern->instructions module body-info pattern next)
    (->instructions module action)))

(defn- ->instructions
  [module term]
  (match term
    {:ast/term  :application
     :function  function
     :arguments arguments}
    (let [arguments*
          (->> arguments
            (rseq)
            (mapcat (partial ->instructions module)))
          function* (lookup-function module (:symbol function))]
      (conj
        (function* arguments*)
        [(return (lookup-type module (:type-checker/type term)))]))

    {:ast/term :match :body body :branches branches}
    (let [type      (lookup-type module (:type-checker/type body))
          body-info {:register (next-register module type) :type type}
          marks     (repeatedly (inc (count branches)) #(next-mark module))]
      (utils/concatv
        (->instructions module body)
        [[(store type) (:register body-info)]]
        (mapcat
          (partial branch->instructions module body-info)
          branches
          (partition 2 1 marks))
        [[:mark (last marks)]
         [(return (lookup-type module (:type-checker/type term)))] ; TODO: throw exception
         ]))

    {:ast/term :symbol :symbol symbol}
    (lookup-binding module symbol)

    {:ast/term :variant :variant [injector value]}
    (let [{:keys [class type]} (lookup-variant module injector)]
      (utils/concatv
        [[:new class]
         [:dup]]
        (->instructions module value)
        [[:invokespecial class :init [type :void]]]))

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
            {:flags #{:public :static}
             :name  (:name name)
             :type  type}))]
    (add-binding module
      name
      [[:getstatic class (:name field) (:type field)]])
    field))

(defn- allocate-registers
  [method]
  (let [argument-registers (->> method
                             :desc
                             (butlast)
                             (map #(if (wide? %) 2 1))
                             (reduce +))]
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
           {:instructions [] :allocated {} :next argument-registers})
         :instructions))))

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

(defn- ->method
  [module definition]
  (-> definition
    (match
      {:ast/definition :constant
       :name           {:reference :variable :name "main"}
       :body           {:ast/term :lambda :body body}}
      (let [instructions (->instructions module body)]
        {:flags #{:public :static}
         :name  "main"
         :desc  [[String] :void]
         :emit  instructions}))
    (allocate-registers)
    (assign-marks)))

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
    {:name    name
     :super   super
     :methods [{:flags #{:public}
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
                :type  type}]}))

(defn- type->classes
  [module {:keys [name body]}]
  (let [classes
        (match body
          {:ast/type :variant :variants variants}
          (let [super {:flags       #{:public :abstract}
                       :name        (subclass module name)
                       :annotations {} ; TODO: generics
                       :methods     [{:flags #{:protected}
                                      :name  :init
                                      :desc  [:void]
                                      :emit [[:aload 0]
                                             [:invokespecial :super :init [:void]]
                                             [:return]]}]}]
            (add-type module body (:name super))
            (conj
              (map (partial variant->class module (:name super)) variants)
              super)))]
    (->> classes
      (map (fn [class] [(:name class) class]))
      (into {}))))

(defn- definition->bytecode
  [module definition]
  (let [class (class-name module)]
    (match definition
      {:ast/definition :type}
      (update module :bytecode merge (type->classes module definition))

      {:ast/definition :constant :body {:ast/term :lambda}}
      (update-in module [:bytecode class :methods] utils/conjv (->method module definition))

      {:ast/definition :constant}
      (update-in module [:bytecode class :fields] utils/conjv (->field module definition))

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
