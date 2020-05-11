(ns lang.module
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [lang.type :as type]))

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

(defn- importer
  [projection-fn module]
  (letfn [(qualify-references [source type]
            (walk/prewalk
              (fn [node]
                (match node
                  {:ast/type :record}
                  (update node :fields
                    #(->> %
                       (map (fn [[k v]] [(cond-> k (not (:in k)) (assoc :in source)) v]))
                       (into (empty %))))

                  {:ast/type :variant}
                  (update node :injectors
                    #(->> %
                       (map (fn [[k v]] [(cond-> k (not (:in k)) (assoc :in source)) v]))
                       (into (empty %))))

                  _ node))
              type))]
    (->> module
      :imports
      (map
        (fn [import]
          (->> import
            (projection-fn)
            (mapcat
              (fn [[k v]]
                (->> [(:alias import)
                      (:name import)
                      (when (:open import) :open)]
                  (filter some?)
                  (map (fn [m]
                         [(cond
                            (= :open m)
                            k

                            (map? k)
                            (assoc k :in m)

                            (set? k)
                            (->> k
                              (map #(assoc % :in m))
                              (into (empty k))))
                          (qualify-references m v)])))))
            (into {}))))
      (reduce merge))))

(defn surface-bindings
  [module]
  (:values module))

(defn imported-bindings
  [module]
  (importer surface-bindings module))

(defn all-bindings
  [module]
  (merge
    (imported-bindings module)
    (surface-bindings module)))

(defn surface-types
  [module]
  (:types module))

(defn imported-types
  [module]
  (importer surface-types module))

(defn all-types
  [module]
  (merge
    builtins
    (imported-types module)
    (surface-types module)))

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
    (surface-injectors module)))

(defn surface-fields
  [module]
  (->> module
    :types
    (mapcat (fn [[name type]]
              (->> type
                (type/fields)
                (map (fn [[field _]] [field {:ast/type :named :name name}])))))
    (into {})))

(defn imported-fields
  [module]
  (importer surface-fields module))

(defn all-fields
  [module]
  (merge
    (imported-fields module)
    (surface-fields module)))

(defn surface-macros
  [module]
  (:macros module))

(defn imported-macros
  [module]
  (importer surface-macros module))

(defn all-macros
  [module]
  (merge
    (surface-macros module)
    (imported-macros module)))

(defn macro?
  [module symbol]
  (contains? (all-macros module) symbol))

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

(defn signature
  [module]
  (let [name   (str/join "." (:name (:name module)))
        macros (map (fn [[k v]] (format "  macro (%s %s)"
                                  (:name k)
                                  (str/join " " (map :name (:arguments v)))))
                 (surface-macros module))
        types  (map (fn [[k v]]
                      (if-let [params (type/parameters v)]
                        (format "  type (%s %s)"
                          (:name k)
                          (str/join " " (map (comp :name :reference) params)))
                        (format "  type %s" (:name k))))
                 (surface-types module))
        values (map (fn [[k v]]
                      (format "  %s : %s"
                        (:name k)
                        (type/print (first v))))
                 (surface-bindings module))]
    (->>
      (concat
        macros
        types
        values)
      (cons name)
      (str/join "\n"))))
