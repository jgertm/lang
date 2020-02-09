(ns lang.parser.atom
  (:refer-clojure :exclude (boolean))
  (:require [blancas.kern.core :refer :all]
            [lang.parser.lexer :refer :all]))

(def ^:private unit
  (>> (word "nil") (return {:atom :unit :value nil})))

(def ^:private integer
  (bind [value dec-lit]
    (return {:atom :integer :value value})))

(def ^:private string
  (bind [value string-lit]
    (return {:atom :string :value value})))

(def ^:private boolean
  (bind [value (word "true" "false")]
    (return {:atom :boolean :value (case value
                                     "true" true
                                     "false" false)})))

(def expr
  (<|>
    unit
    integer
    string
    boolean))
