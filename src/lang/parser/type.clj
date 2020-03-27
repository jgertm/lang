(ns lang.parser.type
  (:refer-clojure :exclude [vector])
  (:require [blancas.kern.core :refer :all]
            [lang.parser.lexer :refer :all]
            [lang.parser.reference :as reference]))

(declare expr)

(def ^:private named
  (bind [name reference/type]
    (return {:ast/type :named :name name})))

(def ^:private variant
  (parens
    (bind [_ (sym \|)
           variants (many1 (brackets (<*> reference/keyword (optional (fwd expr)))))]
      (return {:ast/type :variant :variants (into (array-map) variants)}))))

(def ^:private vector
  (brackets
    (bind [inner (fwd expr)]
      (return {:ast/type :vector :inner inner}))))

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
    (<:> variant)
    (<:> vector)
    (<:> function)))
