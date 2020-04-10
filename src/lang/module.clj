(ns lang.module
  (:require [clojure.core.match :refer [match]]
            [lang.utils :as utils :refer [undefined]]))

(def builtins
  {{:reference :type :name "Unit"}   {:body  {:ast/type :primitive :primitive :unit}
                                      :class Void/TYPE}
   {:reference :type :name "String"} {:body  {:ast/type :primitive :primitive :string}
                                      :class java.lang.String}
   {:reference :type :name "Int"}    {:body  {:ast/type :primitive :primitive :integer}
                                      :class Integer/TYPE}
   {:reference :type :name "Bool"}   {:body  {:ast/type :primitive :primitive :boolean}
                                      :class Boolean/TYPE}
   {:reference :type :name "Object"} {:body  {:ast/type :primitive :primitive :object}
                                      :class java.lang.Object}})

(defn imported-bindings
  [module]
  (->> module
    :imports
    (map
      (fn [import]
        (let [module (or (:alias import) (:name import))]
          (->> import
            :values
            (map (fn [[k v]] [(assoc k :in module) v]))
            (into {})))))
    (reduce merge)))

(defn surface-bindings
  [module]
  (:values module))

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
  (->> module
    :imports
    (map
      (fn [import]
        (let [module (or (:alias import) (:name import))]
          (->> import
            (surface-types)
            (map (fn [[k v]] [(assoc k :in module) v]))
            (into {})))))
    (reduce merge)))

(defn all-types
  [module]
  (->> (merge
         builtins
         (imported-types module)
         (surface-types module))
    (map (fn [[k v]] [(select-keys k [:reference :name]) v]))
    (into {})))

(defn surface-injectors
  [module]
  (->> module
    :types
    (mapcat (fn [[name {:keys [injectors] :as type}]]
              injectors))
    (into {})))

(defn imported-injectors
  [module]
  (->> module
    :imports
    (map
      (fn [import]
        (let [module (or (:alias import) (:name import))]
          (->> import
            (surface-injectors)
            (map (fn [[k v]] [(assoc k :in module) v]))
            (into {})))))
    (reduce merge)))

(defn all-injectors
  [module]
  (merge
    (imported-injectors module)
    (surface-injectors module)))

(defn surface-macros
  [module]
  (:macros module))

(defn imported-macros
  [module]
  (->> module
    :imports
    (map
      (fn [import]
        (let [module (or (:alias import) (:name import))]
          (->> import
            (surface-macros)
            (map (fn [[k v]] [(assoc k :in module) v]))
            (into {})))))
    (reduce merge)))

(defn all-macros
  [module]
  (merge
    (surface-macros module)
    (imported-macros module)))

(defn macro?
  [module symbol]
  (contains? (all-macros module) symbol))
