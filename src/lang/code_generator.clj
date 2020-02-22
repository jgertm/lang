(ns lang.code-generator
  (:require [clojure.core.match :refer [match]]
            [insn.core :as insn]
            [clojure.string :as str]))

(defn concatv
  [& collections]
  (vec (apply concat collections)))

(def builtins
  {{:reference :variable :name "println"}
   (fn [args]
     (concatv
       [[:getstatic System "out"]]
       args
       [[:invokevirtual java.io.PrintStream "println" [String 'void]]]))})

(defn- class-name
  [module]
  (symbol (str/join "." (:name (:name module)))))

(defn ->instructions
  [module term]
  (match term
    {:ast/term  :application
     :function  function
     :arguments arguments}
    (let [arguments*
          (->> arguments
            (rseq)
            (mapcat (partial ->instructions module)))
          function* (get builtins (:symbol function))]
      (conj
        (function* arguments*)
        [:return]))

    {:ast/term :symbol :symbol symbol}
    [[:getstatic (class-name module) (:name symbol) String]]

    {:ast/term :atom :atom {:value value}}
    [[:ldc value]]))

(defn ->field
  [module definition]
  (let []
    (match definition
      {:ast/definition :constant
       :name           name
       :body           {:ast/term :atom :atom atom}}
      {:flags #{:public :static}
       :name  (:name name)
       :type  (get {:string String} (:atom atom))
       :value (:value atom)})))

(defn ->method
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

(defn ->class
  [module]
  (reduce
    (fn [class definition]
      (match definition
        {:ast/definition :constant :body {:ast/term :lambda}}
        (update class :methods conj (->method module definition))

        {:ast/definition :constant}
        (update class :fields conj (->field module definition))))
    {:name    (class-name module)
     :methods []
     :fields  []}
    (:definitions module)))

(defn run
  [module]
  (let [bytecode [(->class module)]]
    (clojure.pprint/pprint bytecode)
    (run!
      #(-> %
         (insn/visit)
         (insn/write "out/"))
      bytecode)
    (assoc module :bytecode bytecode)))

(comment

  (-> "../lang/examples/hello.lang"
    (lang.compiler/run)
    :bytecode)

  )
