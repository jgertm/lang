(ns lang.parser.type
  (:refer-clojure :exclude [])
  (:require [blancas.kern.core :refer :all]
            [lang.parser.lexer :refer :all]
            [lang.parser.reference :as reference]))

(declare expr)

(def ^:private named
  (bind [name reference/type]
    (return {:ast/type :named :name name})))

(def ^:private application
  (parens
    (bind [operator named
           parameters (many1 named)]
      (return {:ast/type   :application
               :operator   operator
               :parameters parameters}))))

(def ^:private variant
  (parens
    (bind [_ (sym \|)
           injectors (many1 (brackets (<*> reference/injector (optional (fwd expr)))))]
      (return {:ast/type :variant :injectors (into (array-map) injectors)}))))

(def ^:private record
  (braces
   (bind [fields (many1 (<*> reference/field (fwd expr)))]
     (return {:ast/type :record :fields (into (array-map) fields)}))))

(def ^:private function
  (parens
    (bind [_ (word "->")
           domains (many1 (fwd expr))]
      (return (reduce (fn [inner type]
                        {:ast/type :function
                         :domain   type
                         :return   inner})
                (rseq domains))))))

(def expr
  (<|>
    named
    (<:> application)
    (<:> variant)
    (<:> record)
    (<:> function)))
