(ns lang.code-generator.jvm
  (:refer-clojure :exclude [emit-term])
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [insn.core :as insn]
            [insn.util :refer [label]]
            [lang.jvm :as jvm]
            [lang.module :as module]
            [lang.pattern :as pattern]
            [lang.scope :as scope]
            [lang.term :as term]
            [lang.type :as type]
            [lang.utils :refer [undefined]]
            [taoensso.timbre :as log])
  (:import [java.lang.invoke CallSite LambdaMetafactory MethodHandle MethodHandles$Lookup MethodType]))

(def modules (atom {}))


(defn- new-state []
  {::paths     '()
   ::types
   (scope/merge
     (scope/empty)
     (->> module/builtins
       (keep (fn [[type primitive]]
               (when-let [class (get jvm/primitives primitive)]
                 [type {:class class}])))
       (into {})))
   ::values    (scope/empty)
   ::registers '()
   :classes    {}})

(defn- current-unit-path
  [{::keys [paths]}]
  (peek paths))

(defn- current-unit
  [state]
  (get-in state (current-unit-path state)))

(defn- current-class-path
  [state]
  (->> state current-unit-path (take 2) (vec)))

(defn- current-class
  [state]
  (get-in state (current-class-path state)))

(defn- class-name
  ([name] (class-name name ""))
  ([name super]
   (munge
     (case (:reference name)
       :module
       (str/join "." (:name name))

       (:type :injector)
       (format "%s.%s"
         (or (not-empty super) (class-name (:in name)))
         (:name name))))))

(defn- push-class
  [state {:keys [name super] :as class}]
  (let [name (class-name name super)]
    (-> state
      (assoc-in [:classes name]
        (update (merge
                  {:flags   #{:public :final}
                   :version 8
                   ::name   (:name class)}
                  class
                  {:name name})
          :flags conj :super))
      (update ::values scope/enter)
      (update ::paths conj [:classes name]))))

(defn- push-subclass
  [state class]
  (let [superclass (:name (current-class state))]
    (push-class state (assoc class :super superclass))))

(defn- get-class
  [state class]
  (get-in state [:classes class]))

(defn- update-unit
  [state f & args]
  (apply update-in state (current-unit-path state) f args))

(defn- pop-class
  [state]
  (let [class (:name (current-class state))]
    (-> state
      (update-unit
        (fn [cl]
          (cond-> cl
            (not-empty (get-in cl [:methods :clinit :emit]))
            (update-in [:methods :clinit :emit]
              #(vec (concat % [[:return]]))))))
      (cond-> (< 1 (count (::paths state)))
        (-> 
          (update ::values scope/exit)
          (update ::paths pop))))))

(defn- push-field
  [state {:keys [name] :as field}]
  (let [new-path (conj (current-class-path state) :fields name)]
    (-> state
      (assoc-in new-path field)
      (update ::paths conj new-path))))

(defn- pop-field
  [state]
  (let [class               (:name (current-class state))
        {:keys [name type]} (current-unit state)]
    (-> state
      (update ::paths pop)
      (update-unit
        (fn [cl] 
          (cond-> cl
            (not-empty (get-in cl [:methods :clinit :emit]))
            (update-in [:methods :clinit :emit]
              #(conj % [:putstatic class name type]))))))))

(defn- push-method
  [state {:keys [name desc] :as method}]
  (let [new-path       (conj (current-class-path state) :methods name)
        argument-count (dec (count desc))]
    (-> state
      (update-in new-path merge method)
      (update ::values scope/enter)
      (update ::registers conj (atom (dec argument-count)))
      (update ::paths conj new-path))))

(defn- pop-method
  [state]
  (-> state
    (update ::values scope/exit)
    (update ::registers rest)
    (update ::paths pop)))

(defn- register!
  [state]
  (-> state ::registers (first) (swap! inc)))

(defn- bind-symbols
  [state symbols]
  (update state ::values scope/merge symbols))

(defn- lookup-symbol-info
  [{::keys [values]} {:keys [in] :as symbol}]
  (or
    (scope/get values symbol)
    (scope/get (::values (get @modules in)) symbol)
    (log/error "cannot find symbol" symbol)))


(defn- register-type
  [state props]
  (let [class (current-unit state)]
    (update state ::types scope/assoc (::name class)
      (merge
        {:class (:name class)}
        props))))

(defn- register-injector
  [state props]
  (register-type state props))

(defn- register-field
  [state field]
  (let [class               (current-class state)
        {:keys [name type]} (current-unit state)]
    (update state ::types scope/assoc
      field
      {:class (:name class)
       :field (munge name)
       :inner-class type})))

(defn- lookup-type-info
  [{::keys [types]} {:keys [in] :as type}]
  (or
    (scope/get types type)
    (scope/get (::types (get @modules in)) type)))

(defn- specialize-type
  [type]
  (let [object {:ast/type :named
                :name     {:reference :type
                           :name      "Object"
                           :in        {:reference :module
                                       :name      ["lang" "builtin"]}}}
        type   (walk/prewalk
                 #(if (type/universal-variable? %) object %)
                 type)]
    (match type
      {:ast/type :recur :body body}
      (recur body)

      {:ast/type :guarded :body body}
      (recur body)

      {:ast/type :forall :variable variable :body body}
      (recur body)

      _ type)))

(declare as-jvm-desc)
(declare as-function-interface)

(defn- as-type-reference
  [type]
  (match (specialize-type type)
    {:ast/type :named :name name}
    name

    {:ast/type :application :operator operator}
    (recur operator)))

(defn- as-jvm-class
  [state type]
  (letfn [(array? [type]
            (= type
              {:ast/type :named
               :name     {:reference :type
                          :name      "Array"
                          :in        {:reference :module :name ["lang" "builtin"]}}}))]
    (let [type (specialize-type type)]
      (match type
        {:ast/type :named :name name}
        (:class (lookup-type-info state name))

        {:ast/type :application :operator operator :parameters parameters}
        (if (array? operator)
          (Class/forName (format "[L%s;" (as-jvm-class state (first parameters))))
          (recur state operator))

        {:ast/type :function}
        (->> type
          (as-jvm-desc state)
          (as-function-interface state)
          :interface-name)

        {:ast/type :record :fields fields}
        (:class (some #(lookup-type-info state %) (keys fields)))

        {:ast/type :variant :injectors injectors}
        (:class (some #(lookup-type-info state %) (keys injectors)))
        _
        (do (log/error "cannot translate type" type)
            ::fixme.as-jvm-class)))))

(defn- as-jvm-desc
  [state type]
  (match (specialize-type type)
    {:ast/type :function :domain domain :return return}
    (let [domain (as-jvm-class state domain)
          return (if (-> return :ast/type (= :function))
                   (as-jvm-desc state return)
                   (as-jvm-class state return))]
      (if (vector? return)
        (into [domain] return)
        [domain return]))))

(defn- as-function-interface
  [state desc]
  (let [argument-count (count (butlast desc))
        return-type    (last desc)
        interface-name      (format "lang.function.%s%d"
                              (if (= 'void return-type)
                                "Consumer"
                                "Function")
                              argument-count)
        interface-desc (mapv #(if (not= % 'void) Object %) desc)]
    {:interface-name interface-name
     :interface-desc interface-desc}))

(defn- emit
  [state & bytecode]
  {:pre [(every? (some-fn vector? nil?) bytecode)]}
  (let [path     (current-unit-path state)
        bytecode (->> bytecode
                   (reduce concat)
                   (filterv some?))]
    (if (= :fields (get path 2))
      (-> state
        (push-method {:flags #{:static}
                      :name  :clinit
                      :desc  ['void]})
        (emit bytecode)
        (pop-method))
      (update-in state (conj path :emit) (fnil into []) bytecode))))

(defn- reduce-state
  [state op coll]
  (reduce op state coll))


(declare emit-term)

(defmulti emit-pattern
  (fn [state next form] (pattern/is? form)))

(defmethod emit-pattern :symbol
  [state {:keys [body]} {:keys [symbol type-checker.pattern/type] :as form}]
  (bind-symbols state {symbol {:instruction body
                               :class       (as-jvm-class state type)}}))

(defmethod emit-pattern :wildcard
  [state params form]
  state)

(defmethod emit-pattern :atom
  [state {:keys [body next]} {:keys [atom]}]
  (let [compare-insn (case (:atom atom)
                       :integer
                       [:invokevirtual BigInteger "compareTo" [BigInteger Integer/TYPE]]
                       :boolean
                       [:invokevirtual Boolean "compareTo" [Boolean Integer/TYPE]]

                       :string
                       [:invokevirtual String "compareTo" [String Integer/TYPE]])]
    (-> state
      (emit [body])
      (emit-term {:ast/term :atom :atom atom})
      (emit [compare-insn
             [:ifne next]]))))

(defmethod emit-pattern :record
  [state {:keys [body next]} {:keys [fields type-checker.pattern/type] :as form}]
  (reduce-state state
    (fn [state [field sub-pattern]]
      (let [{:keys [class field inner-class]}
            (lookup-type-info state field)
            inner-register (register! state)]
        (-> state
          (emit [body
                 [:getfield class field inner-class]
                 [:astore inner-register]])
          (emit-pattern
            {:next next :body [:aload inner-register]}
            sub-pattern))))
    fields))

(defmethod emit-pattern :variant
  [state {:keys [next body]} {:keys [injector value type-checker.pattern/type] :as form}]
  (let [{:keys [class inner-class]} (lookup-type-info state injector)
        inner-register              (when (some? value) (register! state))] 
    (-> state
      (emit [body
             [:instanceof class]
             [:ifeq next]])
      (cond-> (some? value)
        (->
          (emit [body
                 [:checkcast class]
                 [:getfield class :value inner-class]
                 [:astore inner-register]])
          (emit-pattern
            {:next next :body [:aload inner-register]}
            value))))))

(defmethod emit-pattern :default ; FIXME(tjgr): rm
  [state _next form]
  (log/error "cannot emit pattern" (:ast/pattern form))
  state)


(defmulti emit-term
  (fn [state form] (term/is? form)))

(defmethod emit-term :lambda
  [state {:keys [name name-resolution/captured-symbols type-checker.term/type] :as form}]
  (let [captured-symbols
        (vec captured-symbols)
        capture-bindings
        (->> (range)
          (map
            (fn [symbol register]
              [symbol (merge
                        (lookup-symbol-info state symbol)
                        {:instruction [:aload register]})])
            captured-symbols)
          (into {}))
        {:keys [arguments body]}
        (->> form
          (iterate #(when (term/lambda? %) (:body %)))
          (take-while some?)
          (reduce
            (fn [acc {:keys [ast/term argument type-checker.term/type] :as form}]
              (case term
                :lambda (update acc :arguments (fnil conj []) [argument (:domain (specialize-type type))])
                (assoc acc :body form)))
            nil))
        argument-bindings
        (->> arguments
          (map-indexed
            (fn [index [symbol type]]
              [(select-keys symbol [:reference :name])
               {:instruction [:aload (+ index (count captured-symbols))]
                :class       (as-jvm-class state type)}]))
          (into {}))
        class        (:name (current-class state))
        method-name  (munge (or (:name name) (gensym "fn")))
        bindings     (merge capture-bindings argument-bindings)
        capture-desc (mapv #(:class (lookup-symbol-info state %)) captured-symbols)
        lambda-desc  (as-jvm-desc state type)
        desc         (into capture-desc lambda-desc)
        return-type  (last lambda-desc)
        return-insn  (case return-type
                       'void :return
                       :areturn)
        method-interface
        (as-function-interface state desc)
        callsite-interface
        (->> type
          (as-jvm-desc state)
          (as-function-interface state))]
    (-> state
      (cond-> (some? name) ; initial type for the field has no knowlege of captures
        (-> 
          (update-unit assoc :type (:interface-name method-interface))
          (update ::values scope/assoc name
            (-> state
              ::values
              (scope/get name)
              (assoc-in [:instruction 3] (:interface-name method-interface))
              (assoc :class (:interface-name method-interface))))))
      (push-method {:flags (if (some? name) #{:public :static} #{:private :static})
                    :name  method-name
                    :desc  desc})
      (bind-symbols bindings)
      (emit-term body)
      (emit [[return-insn]])
      (pop-method)
      ;; refer to https://docs.oracle.com/javase/8/docs/api/java/lang/invoke/LambdaMetafactory.html for details
      (reduce-state
        (fn [state symbol]
          (emit state [(:instruction (lookup-symbol-info state symbol))]))
        captured-symbols)
      (emit [[:invokedynamic
              "apply" ; invokedName
              (conj capture-desc (:interface-name callsite-interface)) ; invokedType, captured values go here
              [:invokestatic
               LambdaMetafactory
               "metafactory"
               [MethodHandles$Lookup String MethodType MethodType MethodHandle MethodType CallSite]]
              [(insn.util/method-type (:interface-desc callsite-interface)) ; samMethodType: Signature and return type of method to be implemented by the function object.
               (insn.util/handle :invokestatic class method-name desc) ; implMethod, desc needs to accept captures
               (insn.util/method-type lambda-desc)]]])))) ; instantiatedMethodType

(defmethod emit-term :application
  [state {:keys [function arguments type-checker.term/type] :as form}]
  (let [return-type (as-jvm-class state type)
        {:keys [interface-name interface-desc]}
        (->> function
          :type-checker.term/type
          (as-jvm-desc state)
          (as-function-interface state))]
    (-> state
      (emit-term function)
      (reduce-state emit-term arguments)
      (emit [[:invokeinterface
              interface-name
              "apply"
              interface-desc]
             (when (not (jvm/primitive? return-type))
               [:checkcast return-type])]))))

(defmethod emit-term :recur
  [state {:keys [name reference body] :as form}]
  ;; TODO(tjgr): bind reference for recursive anonymous lambdas
  (emit-term state (cond-> body name (assoc :name name))))

(defmethod emit-term :match
  [state {:keys [body branches]}]
  (let [success       (label)
        failure       (label)
        body-register (register! state)]
    (-> state
      (emit-term body)
      (emit [[:astore body-register]])
      (reduce-state
        (fn [state [{:keys [patterns action] :as branch} counter [current next]]]
          (-> state
            (update ::values scope/enter)
            (cond-> (some? current) ; first branch needs no jump target
              (emit [[:mark current]]))
            (reduce-state
              (fn [state pattern]
                (emit-pattern state
                  {:next (if (zero? counter) failure next)
                   :body [:aload body-register]}
                  pattern))
              patterns)
            (emit-term action)
            (emit [[:goto success]])
            (update ::values scope/exit)))
        (->> label
          (repeatedly)
          (cons nil)
          (partition 2 1)
          (map vector
            branches
            (->> branches (count) (range) (reverse)))))
      (emit [[:mark failure]
             [:new Exception] ; TODO(tjgr): more specific exception class
             [:dup]
             [:invokespecial Exception :init ['void]]
             [:athrow]
             [:mark success]]))))

(defmethod emit-term :extract
  [state {:keys [record field] :as form}]
  (let [{:keys [class]}       (lookup-type-info state (as-type-reference (:type-checker.term/type record)) )
        {:keys [inner-class]} (lookup-type-info state field)]
    (-> state
      (emit-term record)
      (emit [[:getfield class (munge (:name field)) inner-class]]))))

(defmethod emit-term :atom
  [state {{:keys [atom value]} :atom}] 
  (case atom
    :integer
    (emit state
      [[:new BigInteger]
       [:dup]
       [:ldc value]
       [:invokespecial BigInteger :init [String 'void]]])

    :string
    (emit state
      [[:ldc value]])

    :boolean
    (emit state
      [[:ldc (case value true 1 false 0)]
       [:invokestatic Boolean "valueOf" [Boolean/TYPE Boolean]]])))

(defmethod emit-term :record
  [state {:keys [fields type-checker.term/type] :as form}]
  (let [class        (->> type (as-jvm-class state) (get-class state))
        record-class (:name class)
        ctor-desc    (get-in class [:methods :init :desc])]
    (-> state
      (emit [[:new record-class]
             [:dup]])
      (reduce-state
        (fn [state [_field value]]
          (emit-term state value))
        fields)
      (emit [[:invokespecial record-class :init ctor-desc]]))))

(defmethod emit-term :variant
  [state {:keys [injector value] :as form}]
  (let [{:keys [class ctor-desc]} (lookup-type-info state (:injector form))]
    (-> state
      (emit
        [[:new class]
         [:dup]])
      (cond-> (some? value)
        (emit-term value))
      (emit
        [[:invokespecial class :init ctor-desc]]))))

(defmethod emit-term :symbol
  [state {:keys [symbol type-checker.term/type]}]
  (cond
    (jvm/native? symbol)
    (let [{:keys [name in]} symbol
          class             (->> in :name (str/join "."))
          inner-class       (:class type)]
      (emit state [[:getstatic class name inner-class]]))

    :else
    (emit state [(:instruction (lookup-symbol-info state symbol))])))

(defmethod emit-term :sequence
  [state {:keys [operations]}]
  (reduce emit-term state operations))

(defmethod emit-term :access
  [state {:keys [object field] :as form}]
  (match field
    {:ast/term :application :function function :arguments arguments}
    (let [class     (->> function :symbol :in :name (str/join "."))
          method    (:name (:symbol function))
          signature (:signature (:type-checker.term/type function))]
      (-> state
        (emit-term object)
        (reduce-state emit-term arguments)
        (emit [[:invokevirtual class method signature]])))))

(defmethod emit-term :default ; FIXME(tjgr): rm
  [state form]
  (log/error "cannot emit term" (:ast/term form))
  state)


(defmulti compile-type
  (fn [state form]
    (type/is? form)))

(defmethod compile-type :variant
  [state {:keys [injectors]}]
  (let [super-ctor-desc ['void]]
    (-> state
      (register-type {:ctor-desc super-ctor-desc})
      (update-unit update :flags
        #(-> %
           (disj :final)
           (conj :abstract)))
      (push-method {:flags #{:public}
                    :name  :init
                    :desc  super-ctor-desc})
      (emit [[:aload 0]
             [:invokespecial :super :init super-ctor-desc]
             [:return]])
      (pop-method)
      (reduce-state
        (fn [state [injector inner-type]]
          (let [inner-class-name   (promise)
                state
                (-> state
                  (push-subclass {:name injector})
                  (cond->
                      (-> inner-type :ast/type (= :record))
                    (-> 
                      (push-class {:name {:reference :type
                                          :name      (str (gensym "type"))
                                          :in        injector}})
                      (update-unit (fn [class] (deliver inner-class-name (:name class)) class))
                      (compile-type inner-type)
                      (pop-class))))
                inner-class        (if (realized? inner-class-name)
                                     @inner-class-name
                                     (some->> inner-type (as-jvm-class state)))
                injector-ctor-desc (filterv some? [inner-class 'void])]
            (-> state
              (register-injector {:ctor-desc   injector-ctor-desc
                                  :inner-class inner-class})
              (cond-> inner-class
                (->
                  (push-field {:flags #{:public :final}
                               :name  :value
                               :type  inner-class})
                  (pop-field)))
              (push-method {:flags #{:public}
                            :name  :init
                            :desc  injector-ctor-desc})
              (emit
                [[:aload 0]
                 [:invokespecial :super :init super-ctor-desc]]
                (when inner-type
                  [[:aload 0]
                   [:aload 1]
                   [:putfield :this :value inner-class]])
                [[:return]])
              (pop-method)
              (pop-class))))
        injectors))))

(defmethod compile-type :record
  [state {:keys [fields]}]
  (let [ctor-desc (conj
                    (mapv (partial as-jvm-class state) (vals fields))
                    'void)]
    (-> state
      (register-type {:ctor-desc ctor-desc})
      (reduce-state
        (fn [state [{:keys [name] :as field} inner-type]]
          (let [inner-class (as-jvm-class state inner-type)]
            (-> state
              (push-field {:flags #{:public :final}
                           :name  (munge name)
                           :type  inner-class})
              (register-field field)
              (pop-field))))
        fields)
      (push-method {:flags #{:public}
                    :name :init
                    :desc ctor-desc})
      (emit [[:aload 0]
             [:invokespecial :super :init ['void]]])
      (reduce-state
        (fn [state [index [{:keys [name]} inner-type]]]
          (let [inner-class (as-jvm-class state inner-type)]
            (emit state
              [[:aload 0]
               [:aload (inc index)]
               [:putfield :this (munge name) inner-class]])))
        (map vector (range) fields))
      (emit [[:return]])
      (pop-method))))

(defmethod compile-type :default ; FIXME(tjgr): rm
  [state type]
  (do (log/error "cannot compile type" (:ast/type type))
      state))


(defn compile-definition
  [state {:keys [name] :as definition}]
  (match definition
    {:ast/definition :module :definitions definitions}
    (do (log/debug "compiling module" name)
        (-> state
          (push-class {:name name})
          (reduce-state compile-definition definitions)
          (pop-class)))

    {:ast/definition :constant :body ({:type-checker.term/type type} :as body)}
    (let [class (:name (current-class state))
          type  (as-jvm-class state type)]
      (log/debug "compiling constant" name)
      (-> state
        (bind-symbols {name {:instruction [:getstatic class (munge (:name name)) type]
                             :class       type}})
        (push-field {:flags #{:public :static}
                     :name  (munge (:name name))
                     :type  type})
        (emit-term (assoc body :name name))
        (pop-field)))

    {:ast/definition :type :body body}
    (do (log/debug "compiling type" name)
        (-> state
          (push-class {:flags #{:public :final}
                       :name  name})
          (compile-type (specialize-type body))
          (pop-class)))

    _ state))

(defn run
  ([module]
   (run module false))
  ([module emit?]
   (let [valsv       (comp vec vals)
         classloader (ClassLoader/getSystemClassLoader)
         state       (compile-definition (new-state) module)
         bytecode    (->> state
                       :classes
                       (valsv)
                       (mapv
                         (fn [class]
                           (-> class
                             (update :fields  valsv)
                             (update :methods valsv)))))]
     (swap! modules assoc (:name module) state)
     (when emit?
       (run!
         (fn [class]
           (log/debug "emitting class" (:name class))
           (let [class (insn/visit class)]
             (insn/define classloader class)
             (insn/write class "out/")))
         bytecode))
     (assoc module :bytecode bytecode))))

(comment

  (do (println "\n---")
      (->
        (lang.test-prelude/run :desugar
          (defmodule lang.desugar.typeclasses-test.option
            (:skip-implicits))
          (deftype (Option T)
              (| [:none]
                [:some T]))
          (defclass (Show T)
            (show :$ (-> T String)))
          (definstance (Show (Option T))
            :when (Show T)
            (show [option]
              (match option
                [:some value] (show value)
                [:none] "[:none]"))))
        (run)
        #_:definitions))


  (com.gfredericks.debug-repl/unbreak!!)

  )

;; TODO(tjgr): do we really need this?
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
