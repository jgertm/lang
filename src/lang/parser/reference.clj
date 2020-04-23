(ns lang.parser.reference
  (:refer-clojure :exclude [keyword type])
  (:require [blancas.kern.core :refer :all]
            [lang.parser.lexer :refer :all]))

(def module-name
  (bind [name (sep-by1 (sym* \.) identifier)] 
    (return {:reference :module :name name})))

(def module
  (lexeme module-name))

(def ^:private type-name
  (bind [name (<+> upper (many letter))]
    (return {:reference :type :name name}))) 

(def type
  (lexeme type-name))

(def ^:private variable-name
  (let [local (bind [name identifier]
                (return {:reference :variable :name name}))
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
      (return (assoc name :reference :keyword)))))

(def injector
  (<$> #(assoc % :reference :injector) keyword))

(def field
  (<$> #(assoc % :reference :field) keyword))
