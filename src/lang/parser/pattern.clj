(ns lang.parser.pattern
  (:refer-clojure :exclude [symbol atom])
  (:require [blancas.kern.core :refer :all]
            [lang.parser.atom :as atom]
            [lang.parser.lexer :refer :all]
            [lang.parser.reference :as reference]))

(declare expr)

(def ^:private wildcard
  (>> (sym \_) (return {:ast/pattern :wildcard})))

(def ^:private symbol
  (bind [symbol reference/variable]
    (return {:ast/pattern :symbol :symbol symbol})))

(def ^:private variant
  (brackets
    (bind [variant (<*> reference/keyword (fwd expr))]
      (return {:ast/pattern :variant :variant variant}))))

(def ^:private atom
  (bind [atom atom/expr] 
    (return {:ast/pattern :atom :atom atom})))

(def expr
  (<|>
    wildcard
    symbol
    variant
    atom))
