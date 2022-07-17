(ns lang.module
  (:refer-clojure :exclude [find get])
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [lang.type :as type]
            [lang.state :as state]
            [lang.utils :as utils :refer [undefined]]))

(def builtins
  (->>
    {{:ast/reference :type :name "Unit"}    {:ast/type :primitive :primitive :unit}
     {:ast/reference :type :name "String"}  {:ast/type :primitive :primitive :string}
     {:ast/reference :type :name "Integer"} {:ast/type :primitive :primitive :integer}
     {:ast/reference :type :name "int"}     {:ast/type :primitive :primitive :int}
     {:ast/reference :type :name "Bool"}    {:ast/type :primitive :primitive :boolean}
     {:ast/reference :type :name "bool"}    {:ast/type :primitive :primitive :bool}
     {:ast/reference :type :name "Object"}  {:ast/type :primitive :primitive :object}
     {:ast/reference :type :name "Array"}
     (let [universal-variable (gensym)]
       {:ast/type :forall
        :variable universal-variable
        :body     {:ast/type :primitive :primitive :array :element universal-variable}})}
    (map (fn [[k v]] [(assoc k :in {:ast/reference :module :name ["lang" "builtin"]}) v]))
    (into {})))

(defn dequalifier
  [projection-fn module]
  (->> module
    (projection-fn)
    (mapcat (fn [[k v]]
              (cond
                (set? k) ; records/variants
                (let [k* (into (empty k) (map #(dissoc % :in) k))]
                  [[k* k]
                   [k v]])

                (vector? k) ; typeclass dictionary instance
                [[k v]]

                :else 
                (let [k* (dissoc k :in)]
                  [[k* k]
                   [k v]]))))
    (into {})))

(defn importer
  [projection-fn module]
  (->>
   (state/get! [module :imports])
   (map
    (fn [import]
      (match import
             {:open true}
             (dequalifier projection-fn (:module import))

             {:alias alias}
             (->> (:module import)
                  (dequalifier projection-fn)
                  (map (fn [[k v]]
                         [(cond
                            (set? k) ; records/variants
                            (->> k
                                 (map #(assoc % :in alias))
                                 (into (empty k)))

                            (vector? k) ; typeclass dictionary instance
                            k

                            (not (:in k))
                            (assoc k :in alias)

                            :else k)
                          v]))
                  (into {})))))
   (apply utils/deep-merge)))

(defn surface-symbols
  [module]
  (->> (state/get! [module :parser/ast :definitions])
       (mapcat
        (fn [{type :ast/definition :keys [name] :as definition}]
          (concat
           [(when (contains? #{:typeclass :constant :type :macro} type)
              [name (assoc name
                           :ast/reference type
                           :in module)])]
           (when (= :typeclass type)
             (->> definition
                  :fields
                  keys
                  (map (fn [field] [field (assoc field :in module)])))))))
       (into {})))

(defn imported-symbols
  [module]
  (importer surface-symbols module))

(defn all-symbols
  [module]
  (merge
   (imported-symbols module)
   (surface-symbols module)))

(defn type-of
  [{:keys [in] :as symbol}]
  (some
   #(and (-> :name (= symbol)) %)
   (:definitions (state/get! [in :type-checker/ast]))))

(comment

  (all-symbols {:ast/reference :module :name ["lang" "core"]})

  )

(defn- filter-type
  [type table]
  (->> table
       (filter (fn [[k _]] (-> k :ast/reference (= type))))
       (into (empty table))))

(defn surface-typeclasses
  [module]
  (filter-type :typeclass (surface-symbols module)))

(defn all-typeclasses
  [module]
  (filter-type :typeclass (all-symbols module)))

(defn surface-macros
  [module]
  (filter-type :macro (surface-symbols module)))

(defn all-macros
  [module]
  (filter-type :macro (all-symbols module)))

(defn macro?
  [module symbol]
  (contains? (all-macros module) symbol))

(defn surface-bindings
  [module]
  (filter-type :constant (surface-symbols module)))

(defn all-bindings
  [module]
  (filter-type :constant (all-symbols module)))

(defn surface-types
  [module]
  (filter-type :type (surface-symbols module)))

(defn all-types
  [module]
  (filter-type :type (all-symbols module)))

(defn surface-injectors
  [module]
  (->> module
    surface-types
    (mapcat (fn [[name type]]
              (->> type
                (type/injectors)
                (map (fn [[injector _]] [injector {:ast/type :named :name name}])))))
    (into {})))

(defn imported-injectors
  [module]
  (importer surface-injectors module))

(defn all-injectors
  [module]
  (merge
    (imported-injectors module)
    (dequalifier surface-injectors module)))

(defn surface-fields
  [module]
  (->> module
    :types
    (mapcat (fn [[name type]]
              (->> type
                (type/fields)
                (map (fn [[field _]] [field {:ast/type :named :name name}])))))
    (into {})))

(defn inner-fields
  [module]
  (->> module
    :types
    (mapcat
      (fn [[name type]]
        (->> type
          (type/nodes)
          (filter
            (every-pred
              (partial not= type)
              #(-> % :ast/type (= :record))))
          (mapcat
            (fn [index inner-type]
              (let [inner-type-name (update name :name #(format "%s$inner_%d" % index))]
                (map
                  (fn [inner-field]
                    [inner-field {:ast/type :named :name inner-type-name}])
                  (keys (type/fields inner-type)))))
            (range)))))
    (into {})))

(defn imported-fields
  [module]
  (importer surface-fields module))

(defn all-fields
  [module]
  (merge
    (importer inner-fields module)
    (imported-fields module)
    (dequalifier inner-fields module)
    (surface-fields module)))

(defn surface-typeclass-dictionaries
  [module]
  @(:desugar.typeclasses/dictionary-instances module))

(defn imported-typeclass-dictionaries
  [module]
  (->> module
       (importer surface-typeclass-dictionaries)
       (map (fn [[k v]] [(dissoc k :in) v]))
       (into {})))

(defn all-typeclass-dictionaries
  [module]
  (merge
    (surface-typeclass-dictionaries module)
    (imported-typeclass-dictionaries module)))

(defn surface-classes
  [module]
  (:classes module))

(defn imported-classes
  [module]
  (importer surface-classes module))

(defn all-classes
  [module]
  (merge
    (surface-classes module)
    (imported-classes module)))

(defn surface-instructions
  [module]
  (:instructions module))

(defn imported-instructions
  [module]
  (importer surface-instructions module))

(defn all-instructions
  [module]
  (merge
    (surface-instructions module)
    (imported-instructions module)))

(defn find
  [table symbol]
  (let [entry (clojure.core/get table symbol)]
    (match entry
      ({:ast/reference _ :in _} :as qualified-reference)
      (recur table qualified-reference)

      nil nil

      :else [symbol entry])))

(defn get
  [table symbol]
  (second (find table symbol)))

(defn signature
  [module]
  (let [name   (str/join "." (:name (:name module)))
        macros (->> module
                 (surface-macros)
                 (map (fn [[k v]] (format "  macro (%s %s)"
                                    (:name k)
                                    (str/join " " (map :name (:arguments v)))))))
        types  (map (fn [[k v]]
                      (if-let [params (type/parameters v)]
                        (format "  type (%s %s)"
                          (:name k)
                          (str/join " " (map (comp :name :ast/reference) params)))
                        (format "  type %s" (:name k))))
                 (surface-types module))
        typeclasses (->> module
                      (surface-typeclasses)
                      (filter (fn [[k v]] (= (:in k) (:name module))))
                      (mapcat (fn [[k v]]
                                (cons
                                  (format "  typeclass %s"
                                    (:name k))
                                  (map
                                    (fn [[k v]]
                                      (format "    %s : %s"
                                        (:name k)
                                        (type/print v)))
                                    (:fields v))))))
        values (->> module
                 (surface-bindings)
                 (map (fn [[k v]]
                        (format "  %s : %s"
                          (:name k)
                          (type/print (first v))))))]
    (->>
      (concat
        macros
        types
        typeclasses
        values)
      (cons name)
      (str/join "\n"))))
