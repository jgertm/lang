(ns lang.parser.definition
  (:refer-clojure :exclude [type])
  (:require [blancas.kern.core :refer :all]
            [clojure.string :as str]
            [lang.parser.lexer :refer :all]
            [lang.parser.reference :as reference]
            [lang.parser.term :as term]
            [lang.parser.type :as type]))

(declare expr)

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
             skip-implicits (optional (<:> (parens (>> (word ":skip-implicits") (return true)))))
             imports (optional (<:> (parens (>> (word ":import") (many import)))))
             definitions (many (fwd expr))]
        (return {:ast/definition :module
                 :name           name
                 :skip-implicits skip-implicits
                 :imports        imports
                 :definitions    definitions})))))

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
             operations (many1 term/expr)]
        (return {:ast/definition :constant
                 :name           name
                 :body
                 {:ast/term  :recur
                  :reference name
                  :body
                  (->> arguments
                    (rseq)
                    (reduce
                      (fn [term arg]
                        {:ast/term :lambda
                         :argument arg
                         :body     term})
                      {:ast/term   :sequence
                       :operations operations}))}})))))

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

(def ^:private typeclass
  (parens
    (bind [_ (word "defclass")
           [name params]
           (parens (<*> reference/typeclass (many1 reference/type)))
           fields
           (<$> (partial into (array-map))
             (many1 (parens (<*>
                              reference/variable
                              (>> (sym \:) type/expr)))))]
      (return {:ast/definition :typeclass
               :name           name
               :params         params
               :fields         fields}))))

(def ^:private typeclass-instance
  (let [field
        (parens
          (bind [name reference/variable
                 arguments (brackets (many0 reference/variable))
                 operations (many1 term/expr)]
            (return [name
                     (->> arguments
                       (rseq)
                       (reduce
                         (fn [term arg]
                           {:ast/term :lambda
                            :argument arg
                            :body     term})
                         {:ast/term   :sequence
                          :operations operations}))])))]
    (parens
      (bind [_ (word "definstance")
             [name types]
             (parens (<*> reference/typeclass (many1 type/expr)))
             fields
             (<$> (partial into (array-map))
               (many1 field))]
        (return {:ast/definition :typeclass-instance
                 :name           name
                 :types          types
                 :fields         fields})))))

(def expr
  (<|>
    (<:> module)
    (<:> type)
    (<:> constant)
    (<:> function)
    (<:> macro)
    (<:> typeclass)
    (<:> typeclass-instance)))

(comment

  (blancas.kern.core/run*
    expr
    (str/trim "
(definstance (Show Bool)
  (show [b]
    (match b
      true  \"true\"
      false \"false\")))
"))


  )
