(ns lang.parser.definition
  (:refer-clojure :exclude [type])
  (:require [blancas.kern.core :refer :all]
            [lang.parser.lexer :refer :all]
            [lang.parser.reference :as reference]
            [lang.parser.term :as term]
            [lang.parser.type :as type]))

(def ^:private module
  (let [import (brackets
                 (bind [name reference/module
                        alias (optional (>> (word ":as") reference/module))]
                   (return (merge
                             {:module name}
                             (when alias {:alias alias})))))]
    (parens
      (bind [_ (word "defmodule")
             name reference/module
             imports (optional (parens (>> (word ":import") (many import))))]
        (return {:ast/definition :module
                 :name           name
                 :imports        imports})))))

(def ^:private type
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

(def ^:private constant
  (parens
    (bind [_ (word "def")
           name reference/variable
           body term/expr]
      (return {:ast/definition :constant
               :name           name
               :body           body}))))

(def ^:private function
  (let [argument (bind [symbol reference/variable
                        type (optional (>> (sym \:) type/expr))]
                   (return
                     (if type
                       (assoc symbol :type type)
                       symbol)))]
    (parens
      (bind [_ (word "defn")
             name reference/variable
             arguments (brackets (many0 argument))
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
                     body))})))))

(def ^:private macro
  (parens
    (bind [_ (word "defmacro")
           name reference/variable
           arguments (brackets (many0 reference/variable))
           body term/expr]
      (return {:ast/definition :macro
               :name           name
               :arguments      arguments
               :body           body}))))

(def expr
  (<|>
    (<:> module)
    (<:> type)
    (<:> constant)
    (<:> function)
    (<:> macro)))
