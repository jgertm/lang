(ns lang.code-generator.jvm
  (:refer-clojure :exclude [emit-term])
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [insn.core :as insn]
            [insn.util :refer [label]]
            [lang.definition :as definition]
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
          (->> parameters
            (first)
            (as-jvm-class state)
            (.getName)
            (format "[L%s;")
            (Class/forName))
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
                              (if (= Void/TYPE return-type)
                                "Consumer"
                                "Function")
                              argument-count)
        interface-desc (mapv #(if (not= % Void/TYPE) Object %) desc)]
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
                      :desc  [Void/TYPE]})
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
  [state {:keys [body next]} {:keys [fields] :as form}]
  (reduce-state state
    (fn [state [field {:keys [type-checker.pattern/type] :as sub-pattern}]]
      (let [{:keys [class field inner-class]}
            (lookup-type-info state field)
            inner-register     (register! state)
            actual-inner-class (as-jvm-class state type)]
        (-> state
          (emit [body
                 [:getfield class field inner-class]
                 [:checkcast actual-inner-class]
                 [:astore inner-register]])
          (emit-pattern
            {:next next :body [:aload inner-register]}
            sub-pattern))))
    fields))

(defmethod emit-pattern :variant
  [state {:keys [next body]} {:keys [injector value type-checker.pattern/type] :as form}]
  (let [{:keys       [class]
         :inner/keys [pseudorecord single]}
        (lookup-type-info state injector)
        inner-register (when (some? value) (register! state))] 
    (-> state
      (emit [body
             [:instanceof class]
             [:ifeq next]])
      (as-> $
          (cond
            (and pseudorecord (pattern/record? value))
            (-> $
              (reduce-state
                (fn [state [field {:keys [type-checker.pattern/type] :as sub-pattern}]]
                  (let [inner-class        (get pseudorecord field)
                        actual-inner-class (as-jvm-class state type)
                        inner-register     (register! state)
                        field              (munge (:name field))]
                    (-> state
                      (emit [body
                             [:checkcast class]
                             [:getfield class field inner-class]
                             [:checkcast actual-inner-class]
                             [:astore inner-register]])
                      (emit-pattern
                        {:next next :body [:aload inner-register]}
                        sub-pattern))))
                (:fields value)))

            single 
            (-> $
              (emit [body
                     [:checkcast class]
                     [:getfield class :value single]
                     [:astore inner-register]])
              (emit-pattern
                {:next next :body [:aload inner-register]}
                value))

            :else $)))))

(defmethod emit-pattern :default ; FIXME(tjgr): rm
  [state _next form]
  (log/error "cannot emit pattern" (:ast/pattern form))
  state)


(defmulti emit-term
  (fn [state form] (term/is? form)))

(defmethod emit-term :lambda
  [state {:keys [name arguments body name-resolution/captured-symbols type-checker.term/type] :as form}]
  (let [captured-symbols (vec captured-symbols)
        capture-desc     (mapv #(:class (lookup-symbol-info state %)) captured-symbols)
        lambda-desc      (as-jvm-desc state type)
        capture-bindings
        (map
          (fn [symbol]
            [symbol (lookup-symbol-info state symbol)])
          captured-symbols)
        lambda-bindings
        (map
          (fn [symbol class]
            [(select-keys symbol [:reference :name])
             {:class class}])
          arguments
          lambda-desc)
        bindings     
        (->>
          (concat capture-bindings lambda-bindings)
          (map-indexed
            (fn [index [symbol props]]
              [symbol (assoc props :instruction [:aload index])]))
          (into {}))
        class            (:name (current-class state))
        method-name      (munge (or (:name name) (gensym "fn")))
        desc             (into capture-desc lambda-desc)
        return-type      (last lambda-desc)
        return-insn      (condp = return-type ; NOTE: https://stackoverflow.com/questions/12028944/clojure-case-statement-with-classes/12029256#12029256
                           Void/TYPE :return
                           ;; TODO(tjgr): primitive returns
                           :areturn)
        method-interface
        (as-function-interface state desc)
        callsite-interface
        (as-function-interface state lambda-desc)]
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
             [:invokespecial Exception :init [Void/TYPE]]
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
       [:invokespecial BigInteger :init [String Void/TYPE]]])

    :string
    (emit state
      [[:ldc value]])

    :boolean
    (emit state
      [[:ldc (case value true 1 false 0)]
       [:invokestatic Boolean "valueOf" [Boolean/TYPE Boolean]]])))

(defmethod emit-term :record
  [state {:keys [fields type-checker.term/type] :as form}]
  (let [{:keys [class ctor-desc]}
        (lookup-type-info state (as-type-reference type))]
    (-> state
      (emit [[:new class]
             [:dup]])
      (reduce-state
        (fn [state [_field value]]
          (emit-term state value))
        fields)
      (emit [[:invokespecial class :init ctor-desc]]))))

(defmethod emit-term :variant
  [state {:keys [injector value] :as form}]
  (let [{:keys       [class ctor-desc]
         :inner/keys [pseudorecord single]}
        (lookup-type-info state injector)]
    (-> state
      (emit
        [[:new class]
         [:dup]])
      (as-> $
          (cond
            pseudorecord
            (reduce-state $ emit-term (vals (:fields value)))

            single
            (emit-term $ value)

            :else $))
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
  [state {:keys [object field]}]
  (match field
    {:ast/term :application :function function :arguments arguments}
    (let [class          (->> function :symbol :in :name (str/join "."))
          method         (:name (:symbol function))
               type           (:type-checker.term/type function)
               signature      (:signature type)
               invoke-insn    (case (:type type)
                                :static :invokestatic
                                :instance :invokevirtual)]
      (-> state
        (emit-term object)
        (reduce-state emit-term arguments)
        (emit [[invoke-insn class method signature]])))))

(defmethod emit-term :default ; FIXME(tjgr): rm
  [state form]
  (log/error "cannot emit term" (:ast/term form))
  state)


(defmulti compile-type
  (fn [state form]
    (type/is? form)))

(defmethod compile-type :variant
  [state {:keys [injectors]}]
  (let [super-ctor-desc [Void/TYPE]]
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
          (let [fields    (when inner-type
                            (->> (or (:fields inner-type)
                                     {{:name "value"} inner-type})
                              (map (fn [[k v]] [k (as-jvm-class state v)]))
                              (into {})))
                ctor-desc (conj (vec (vals fields)) Void/TYPE)]
            (-> state
              (push-subclass {:name injector})
              (register-injector
                (merge {:ctor-desc ctor-desc}
                  (match inner-type
                    {:ast/type :record}
                    {:inner/pseudorecord fields}

                    {:ast/type _}
                    {:inner/single (as-jvm-class state inner-type)}

                    nil nil)))
              (reduce-state
                (fn [state [{:keys [name]} inner-class]]
                  (-> state
                    (push-field {:flags #{:public :final}
                                 :name  (munge name)
                                 :type  inner-class})
                    (pop-field)))
                fields)
              (push-method {:flags #{:public}
                            :name  :init
                            :desc  ctor-desc})
              (emit [[:aload 0]
                     [:invokespecial :super :init [Void/TYPE]]])
              (reduce-state
                (fn [state [index [{:keys [name]} inner-class]]]
                  (emit state
                    [[:aload 0]
                     [:aload (inc index)]
                     [:putfield :this (munge name) inner-class]]))
                (map vector (range) fields))
              (emit [[:return]])
              (pop-method)
              (pop-class))))
        injectors))))

(defmethod compile-type :record
  [state {:keys [fields]}]
  (let [ctor-desc (conj
                    (mapv (partial as-jvm-class state) (vals fields))
                    Void/TYPE)]
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
             [:invokespecial :super :init [Void/TYPE]]])
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
    (do (log/debug "emitting module" (definition/name definition))
        (-> state
          (push-class {:name name})
          (reduce-state compile-definition definitions)
          (pop-class)))

    {:ast/definition :constant :body ({:type-checker.term/type type} :as body)}
    (let [class (:name (current-class state))
          type  (as-jvm-class state type)]
      (log/debug "emitting constant" (definition/name definition))
      (-> state
        (bind-symbols {name {:instruction [:getstatic class (munge (:name name)) type]
                             :class       type}})
        (push-field {:flags #{:public :static}
                     :name  (munge (:name name))
                     :type  type})
        (emit-term (assoc body :name name))
        (pop-field)))

    {:ast/definition :type :body body}
    (do (log/debug "emitting type" (definition/name definition))
        (-> state
          (push-class {:flags #{:public :final}
                       :name  name})
          (compile-type (specialize-type body))
          (pop-class)))

    _ state))

(defn resort
  [classes]
  (let [class-names (->> classes
                      (vals)
                      (map :name)
                      (set))]
    (letfn [(referenced-classes [class]
              (let [constructions
                    (->> class
                      :methods
                      (vals)
                      (mapcat
                        #(->> %
                           :emit
                           (filter (fn [insn]
                                     (and (= :new (first insn))
                                          (string? (second insn)))))
                           (map second)))
                      (set))
                    super (some-> class :super (hash-set))]
                (set/intersection
                  (set/union
                    constructions
                    super)
                  class-names)))
            (index-by-dependencies [classes]
              (->> classes
                (group-by referenced-classes)
                (map (fn [[refs classes]]
                       [refs
                        (set (map :name classes))]))
                (into {})))
            (topological-sort [acc input]
              (if (empty? input)
                acc
                (let [classes (get input #{})]
                  (recur
                    (into acc classes)
                    (->>
                      (dissoc input #{})
                      (reduce-kv
                        (fn [acc k v]
                          (update acc
                            (set/difference k classes)
                            set/union v))
                        {}))))))]

      (->> classes
        (vals)
        (index-by-dependencies)
        (topological-sort [])
        (map (fn [name] (get classes name)))))))

(defn run
  ([module]
   (run module false))
  ([module emit?]
   (let [valsv       (comp vec vals)
         classloader (ClassLoader/getSystemClassLoader)
         state       (compile-definition (new-state) module)
         bytecode    (->> state
                       :classes
                       (resort)
                       (mapv
                         (fn [class]
                           (-> class
                             (update :fields  valsv)
                             (update :methods valsv)))))]
     (swap! modules assoc (:name module) state)
     (when emit?
       (run!
         (fn [class]
           (log/debug "writing classfile" (:name class))
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
        (run :emit)
        #_:definitions))


  (com.gfredericks.debug-repl/unbreak!!)

  (com.gfredericks.debug-repl/unbreak!)

  (letfn [(run [path]
            (-> path
                (lang.compiler/run :until :code-generator)))]
    (do (println "\n–-—")
        (run "std/lang/option.lang")
        (run "std/lang/list.lang")
        (run "std/lang/math.lang")
        (run "std/lang/io.lang")
        (run "std/lang/core.lang")
        #_#_#_(run "examples/option.lang")
        (run "examples/linked-list.lang")
        (run "examples/arithmetic.lang")))

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
