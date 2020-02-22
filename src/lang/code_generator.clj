(ns lang.code-generator
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [insn.core :as insn]
            [lang.utils :as utils]))

(def ^:private builtins
  {:code-generator/methods
   {{:reference :variable :name "println"}
    (fn [args]
      (utils/concatv
        [[:getstatic System "out"]]
        args
        [[:invokevirtual java.io.PrintStream "println" [String 'void]]]))}
   :code-generator/fields {}})

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
    (get primitives type)))

(defn- lookup-method
  [module symbol]
  (get (:code-generator/methods builtins) symbol))

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
          function* (lookup-method module (:symbol function))]
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

(defn- ->class
  [module]
  (reduce
    (fn [class definition]
      (match definition
        {:ast/definition :constant :body {:ast/term :lambda}}
        (update class :methods conj (->method module definition))

        {:ast/definition :constant}
        (update class :fields conj (->field module definition))))
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

  (-> "../lang/examples/hello.lang"
    (lang.compiler/run)
    :bytecode)

  )
