(ns lang.parser.definition
  (:refer-clojure :exclude [type])
  (:require [blancas.kern.core :refer :all]
            [lang.parser.lexer :refer :all]
            [lang.parser.reference :as reference]
            [lang.parser.term :as term]
            [lang.parser.type :as type]))

(def module
  (parens
    (bind [_ (word "defmodule")
           name reference/module]
      (return {:ast/definition :module
               :name       name}))))

(def type
  (let [concrete (bind [name reference/type]
                   (return {:name name}))
        abstract (parens
                   (bind [name reference/type
                          params (many1 reference/type)]
                     (return {:name name :params params})))]
    (parens
      (bind [_ (word "deftype")
             signature (<|> concrete abstract)
             body type/expr]
        (return (merge
                  {:ast/definition :type}
                  signature
                  {:body body}))))))

(def function
  (parens
    (bind [_ (word "defn")
           name reference/variable
           arguments (brackets (many0 reference/variable))
           body term/expr]
      (return {:ast/definition :constant
               :name           name
               :body
               (->> arguments
                 (rseq)
                 (reduce
                   (fn [term arg]
                     {:ast/term :lambda
                      :argument arg
                      :body     term})
                   body))}))))

(def expr
  (<|>
    (<:> module)
    (<:> type)
    function))
