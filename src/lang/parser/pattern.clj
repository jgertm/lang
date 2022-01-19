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
        (return {:ast/pattern :symbol
                 :symbol symbol})))

(def ^:private variant
  (brackets
    (bind [injector reference/injector
           value (optional (fwd expr))]
      (return
        {:ast/pattern :variant
         :injector    injector
         :value       value}))))

(def ^:private record
  (braces
    (bind [fields (many1 (<*> reference/field (fwd expr)))]
      (return {:ast/pattern :record
               :fields      (into (array-map) fields)}))))

(def ^:private atom
  (bind [atom atom/expr] 
    (return {:ast/pattern :atom :atom atom})))

(def expr
  (<|>
    atom
    wildcard
    symbol
    variant
    record))
