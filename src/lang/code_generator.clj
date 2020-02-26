(ns lang.code-generator
  (:refer-clojure :exclude [load])
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [insn.core :as insn]
            [lang.utils :as utils]))

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

(defn- lookup-type
  [module type]
  (let [primitives
        (->> {:string  String ; TODO: complete
              :unit    :void
              :int     :int
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

(defn- lookup-binding
  [module symbol]
  (get @(:code-generator/bindings module) symbol))

(defn- lookup-variant
  [module injector]
  (get @(:code-generator/variants module) injector))

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
          function* (lookup-binding module (:symbol function))]
      (conj
        (function* arguments*)
        [:return]))

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
    (swap! (:code-generator/bindings module) assoc
      name
      [[:getstatic class (:name field) (:type field)]])
    field))

(defn- ->method
  [module definition]
  (match definition
    {:ast/definition :constant
     :name           {:reference :variable :name "main"}
     :body           {:ast/term :lambda :body body}}
    (let [instructions (->instructions module body)]
      {:flags #{:public :static}
       :name  "main"
       :desc  [[String] :void]
       :emit  instructions})))

(defn- register-native
  [module definition]
  (swap! (:code-generator/bindings module) assoc
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
    (swap! (:code-generator/variants module)
      assoc injector {:class name :super super :type type})
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
            (swap! (:code-generator/types module)
              assoc body (:name super))
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
      (update module :bytecode merge (type->classes module definition)) ; TODO

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
        (let [instructions @(:code-generator/static-initializer-instructions class)]
          (-> class
            (dissoc :code-generator/static-initializer-instructions)
            (update :methods conj {:flags #{:public :static}
                                   :name  :clinit
                                   :desc  [:void]
                                   :emit  (utils/concatv
                                            [[:new name]
                                             [:dup]
                                             [:invokespecial name :init [:void]]]
                                            instructions
                                            [[:return]])})))))))

(defn- ->bytecode
  [module]
  (let [name    (class-name module)
        module* (assoc-in module [:bytecode name]
                  {:name name
                   :code-generator/static-initializer-instructions
                   (atom [])})]
    (->
      (reduce
        definition->bytecode
        module*
        (:definitions module))
      (add-static-initializer))))

(defn run
  [module]
  (let [module (->bytecode (merge module
                             {:code-generator/bindings (atom {})
                              :code-generator/types    (atom {})
                              :code-generator/variants (atom {})}))]
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
