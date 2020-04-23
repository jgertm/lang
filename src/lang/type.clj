(ns lang.type
  (:refer-clojure :exclude [contains? print])
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn is?
  [form]
  (:ast/type form))

(defn universally-quantified?
  [type]
  (match type
    {:ast/type :forall} true
    _ false))

(defn existentially-quantified?
  [type]
  (match type
    {:ast/type :exists} true
    _ false))

(defn quantified?
  [type]
  (or
    (universally-quantified? type)
    (existentially-quantified? type)))

(defn polarity
  [type]
  (case (:ast/type type)
    :forall :negative
    :exists :positive
    :neutral))

(defn negative?
  [type]
  (= :negative (polarity type)))

(defn positive?
  [type]
  (= :positive (polarity type)))

(defn parameters
  [type]
  (match type
    {:ast/type :forall :variable variable :body body}
    (cons
      variable
      (parameters body))

    _ nil))

(defn injectors
  [type]
  (match type
    {:ast/type :variant :injectors injectors}
    injectors

    {:ast/type :forall :body body}
    (recur body)

    _ nil))

(defn fields
  [type]
  (match type
    {:ast/type :record :fields fields}
    fields

    {:ast/type :forall :body body}
    (recur body)

    _ nil))

(defn function
  [types]
  (->> types
    (reverse)
    (reduce
      (fn [acc type]
        {:ast/type :function
         :domain   type
         :return   acc}))))

(defn contains?
  [type child]
  (if (= type child)
    child
    (match type
      {:ast/type :variant :injectors injectors}
      (->> injectors
        (vals)
        (filter some?)
        (some #(contains? % child)))

      {:ast/type :record :fields fields}
      (->> fields
        (vals)
        (some #(contains? % child)))

      {:ast/type :application :operator operator :parameters parameters}
      (some #(contains? % child) (conj parameters operator))

      {:ast/type :function :domain domain :return return}
      (or (contains? domain child) (contains? return child))

      {:ast/type :forall :body body}
      (contains? body child)

      {:ast/type :fix :body body}
      (contains? body child)

      _ nil)))

(defn instantiate-universals
  [type instantiations]
  (match [type instantiations]
    [{:ast/type :forall :variable universal-variable :body body}
     [instantiation & more-instantiations]]
    (recur
      (walk/prewalk-replace {universal-variable instantiation} body)
      more-instantiations)

    [_ []]
    type))

(defn print
  [type]
  (letfn [(collect-arguments [type]
            (match type
              {:ast/type :function :domain domain :return return}
              (cons domain (collect-arguments return))

              _ (list type)))
          (collect-quantifiers [type]
            (match type
              {:ast/type :forall :variable variable :body body}
              (merge-with cons
                {:variables variable}
                (collect-quantifiers body))

              _ {:inner type
                 :variables '()}))]
    (match type
      {:ast/type :named :name name}
      (if true
          (:name name))

      {:ast/type :application :operator operator :parameters parameters}
      (format "(%s %s)"
        (print operator)
        (str/join " " (mapv print parameters)))

      {:ast/type :universal-variable :id id}
      (format "%d" id)

      {:ast/type :quote :inner inner}
      (format "(Expr %s)" (print inner))

      {:ast/type :function}
      (->> type
        (collect-arguments)
        (map print)
        (str/join " ")
        (format "(-> %s)"))

      {:ast/type :forall :variable variable :body body}
      (let [{:keys [inner variables]} (collect-quantifiers type)]
        (format "(∀ %s %s)"
          (str/join " " (map print variables))
          (print inner))))))
