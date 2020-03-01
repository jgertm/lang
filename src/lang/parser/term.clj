(ns lang.parser.term
  (:refer-clojure :exclude [symbol atom])
  (:require [blancas.kern.core :refer :all]
            [lang.parser.atom :as atom]
            [lang.parser.lexer :refer :all]
            [lang.parser.pattern :as pattern]
            [lang.parser.reference :as reference]))

(declare expr)

(def ^:private lambda
  (parens
    (bind [_ (word "fn")
           args (brackets (many reference/variable))
           body (fwd expr)]
      (return (->> args
                rseq
                (reduce
                  (fn [expr arg]
                    {:ast/term :lambda
                     :argument arg
                     :body     expr})
                  body))))))

(def ^:private match
  (let [branch (bind [pattern pattern/expr
                      action (fwd expr)]
                 (return {:pattern pattern
                          :action  action}))]
    (parens
      (bind [_ (word "match")
             body (fwd expr)
             branches (many1 branch)]
        (return {:ast/term :match
                 :body     body
                 :branches branches})))))

(def ^:private application
  (parens
    (bind [function (fwd expr)
           arguments (many (fwd expr))]
      (return {:ast/term  :application
               :function  function
               :arguments arguments}))))

(def ^:private variant
  (brackets
    (bind [variant (<*> reference/keyword (fwd expr))]
      (return {:ast/term :variant
               :variant  variant}))))

(def ^:private atom
  (bind [atom atom/expr]
    (return {:ast/term :atom
             :atom     atom})))

(def ^:private symbol
  (bind [symbol reference/variable]
    (return {:ast/term :symbol
             :symbol   symbol})))

(def expr
  (<|>
    (<:> lambda)
    (<:> match)
    application
    variant
    atom
    symbol))

(comment

  (blancas.kern.core/run*
    match
    "(match greeting [:wrap text] (println text))")


  )
