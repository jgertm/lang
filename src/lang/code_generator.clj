(ns lang.code-generator
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [insn.core :as insn]
            [lang.utils :as utils]))

(defn- class-name
  [module]
  (symbol (str/join "." (:name (:name module)))))

(defn- translate-type
  [module type]
  (let [primitives
        (->> {:string  String
              :unit    'void
              :int     'int
              :boolean 'boolean}
          (map (fn [[k v]] [{:ast/type :primitive :primitive k} v]))
          (into {}))]
    (match type
      {:ast/type :primitive}
      (get primitives type)

      {:ast/type :function :domain domain :return return}
      (let [domain* (translate-type module domain)
            return* (translate-type module return)]
        (if (vector? return*)
          (into [domain*] return*)
          (conj [domain*] return*))))))

(defn- lookup-binding
  [module symbol]
  (get @(:code-generator/bindings module) symbol))

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

    {:ast/term :atom :atom {:value value}}
    [[:ldc value]]))

(defn- ->field
  [module definition]
  (let [field
        (match definition
          {:ast/definition :constant
           :name           name
           :body           ({:ast/term :atom :atom atom} :as term)}
          {:flags #{:public :static}
           :name  (:name name)
           :type  (->> term :type-checker/type (translate-type module))
           :value (:value atom)})]
    (swap! (:code-generator/bindings module) assoc
      (:name definition)
      [[:getstatic
        (class-name module)
        (:name field)
        (:type field)]])
    field))

(defn- ->method
  [module definition]
  (match definition
    {:ast/definition :constant
     :name           {:reference :variable :name "main"}
     :body           {:ast/term :lambda :body body}}
    (let [instructions (->instructions module body)]
      {:flags [:public :static]
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
            (translate-type module type)]])))))

(defn- ->class
  [module]
  (reduce
    (fn [class definition]
      (match definition
        {:ast/definition :constant :body {:ast/term :lambda}}
        (update class :methods utils/conjv (->method module definition))

        {:ast/definition :constant}
        (update class :fields utils/conjv (->field module definition))

        {:ast/definition :native}
        (do (register-native module definition )
            class)))
    {:name (class-name module)}
    (:definitions module)))

(defn run
  [module]
  (let [module* (merge module
                  {:code-generator/bindings (atom {})})
        bytecode [(->class module*)]]
    (run!
      #(-> %
         (insn/visit)
         (insn/write "out/"))
      bytecode)
    (assoc module* :bytecode bytecode)))

(comment

  (-> "examples/hello.lang"
    (lang.compiler/run)
    :bytecode)

  )
