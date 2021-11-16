(ns lang.parser.reference
  (:refer-clojure :exclude [keyword type])
  (:require [blancas.kern.core :refer :all]
            [lang.parser.lexer :refer :all]))

(def module-name
  (bind [name (sep-by1 (sym* \.) identifier)] 
    (return {:ast/reference :module :name name})))

(def module
  (lexeme module-name))

(def ^:private type-name
  (bind [name (<+> upper (many0 letter))]
    (return {:ast/reference :type :name name})))

(def type
  (lexeme type-name))

(def typeclass
  (<$> #(assoc % :ast/reference :typeclass) type))

(def ^:private variable-name
  (let [local (bind [name identifier]
                (return {:ast/reference :constant :name name}))
        qualified (bind [path module-name
                         _ (sym* \/)
                         name local]
                    (return (merge name {:in path})))] 
    (<|> (<:> qualified) local)))

(def variable
  (lexeme variable-name))

(def keyword
  (lexeme
    (bind [name (>> (sym* \:) variable-name)]
      (return (assoc name :ast/reference :keyword)))))

(def injector
  (<$> #(assoc % :ast/reference :injector) keyword))

(def field
  (<$> #(assoc % :ast/reference :field) keyword))
