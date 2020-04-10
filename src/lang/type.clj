(ns lang.type
  (:refer-clojure :exclude [print])
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(defn is?
  [form]
  (:ast/type form))

(defn injectors
  [{:keys [name body]}]
  (letfn [(extract [type]
            (match type
             {:ast/type :variant :injectors injectors}
             (->> injectors
               (map (fn [[k v]] [k #:type{:inner v :outer body :name name}]))
               (into {}))

             {:ast/type :forall}
             (recur (:body type))

             _ nil))]
    (extract body)))


(defn function
  [types]
  (->> types
    (reverse)
    (reduce
      (fn [acc type]
        {:ast/type :function
         :domain   type
         :return   acc}))))

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
      {:ast/type :primitive :primitive primitive}
      (str/capitalize (name primitive))

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
