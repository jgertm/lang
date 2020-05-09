(ns lang.parser.atom
  (:refer-clojure :exclude (boolean))
  (:require [blancas.kern.core :refer :all]
            [clojure.string :as str]
            [lang.parser.lexer :refer :all]))

(def ^:private unit
  (>> (word "nil") (return {:atom :unit :value nil})))

(def ^:private integer
  (lexeme
    (bind [sign      (optional (one-of* "+-"))
           magnitude (<+> digit (many0 (<|> digit (sym* \_))))]
      (let [representation (str sign (str/replace magnitude "_" ""))]
        (return {:atom :integer
                 :value representation})))))

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

(comment

  (blancas.kern.core/run*
    integer
    "+22_000")

  )
