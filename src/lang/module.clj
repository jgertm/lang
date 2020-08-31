(ns lang.module
  (:refer-clojure :exclude [find get])
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [lang.type :as type]
            [lang.utils :refer [undefined]]))

(def builtins
  (->>
    {{:reference :type :name "Unit"}    {:ast/type :primitive :primitive :unit}
     {:reference :type :name "String"}  {:ast/type :primitive :primitive :string}
     {:reference :type :name "Integer"} {:ast/type :primitive :primitive :integer}
     {:reference :type :name "int"}     {:ast/type :primitive :primitive :int}
     {:reference :type :name "Bool"}    {:ast/type :primitive :primitive :boolean}
     {:reference :type :name "Object"}  {:ast/type :primitive :primitive :object}
     {:reference :type :name "Array"}
     (let [universal-variable (gensym)]
       {:ast/type :forall
        :variable universal-variable
        :body     {:ast/type :primitive :primitive :array :element universal-variable}})}
    (map (fn [[k v]] [(assoc k :in {:reference :module :name ["lang" "builtin"]}) v]))
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
  (->> module
    :imports
    (map
      (fn [{:keys [name] :as import}]
        (match import
          {:open _}
          (dequalifier projection-fn import)

          {:alias alias}
          (->> import
            (dequalifier projection-fn)
            (map (fn [[k v]]
                   [(if-not (:in k) (assoc k :in alias) k)
                    v]))
            (into {})))))
    (reduce (partial merge-with merge))))

(defn surface-typeclasses
  [module]
  (:typeclasses module))

(defn imported-typeclasses
  [module]
  (importer surface-typeclasses module))

(defn all-typeclasses
  [module]
  (merge-with merge
    (imported-typeclasses module)
    (dequalifier surface-typeclasses module)))

(defn surface-typeclass-fields
  [module]
  (->> module
    :typeclasses
    (vals)
    (mapcat (fn [{:keys [fields]}]
              (map (fn [[name type]] [name [type :principal]]) fields)))
    (into {})))

(defn imported-typeclass-fields
  [module]
  (importer surface-typeclass-fields module))

(defn all-typeclass-fields
  [module]
  (merge
    (imported-typeclass-fields module)
    (dequalifier surface-typeclass-fields module)))

(defn surface-macros
  [module]
  (:macros module))

(defn imported-macros
  [module]
  (importer surface-macros module))

(defn all-macros
  [module]
  (merge
    (imported-macros module)
    (dequalifier surface-macros module)))

(defn macro?
  [module symbol]
  (contains? (all-macros module) symbol))


(defn surface-bindings
  [module]
  (:values module))

(defn imported-bindings
  [module]
  (importer surface-bindings module))

(defn all-bindings
  [module]
  (merge
    (all-typeclass-fields module)
    (all-macros module)
    (imported-bindings module)
    (dequalifier surface-bindings module)))

(defn surface-types
  [module]
  (:types module))

(defn imported-types
  [module]
  (merge 
    builtins
    (->> builtins
      (keys)
      (map (fn [name] [(dissoc name :in) name]))
      (into {}))
    (importer surface-types module)))

(defn all-types
  [module]
  (merge
    (imported-types module)
    (dequalifier surface-types module)))

(defn surface-injectors
  [module]
  (->> module
    :types
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
      ({:reference _ :in _} :as qualified-reference)
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
                          (str/join " " (map (comp :name :reference) params)))
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
