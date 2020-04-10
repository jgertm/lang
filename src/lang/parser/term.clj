(ns lang.parser.term
  (:refer-clojure :exclude [symbol atom quote unquote unquote-splicing])
  (:require [blancas.kern.core :refer :all]
            [lang.parser.atom :as atom]
            [lang.parser.lexer :refer :all]
            [lang.parser.pattern :as pattern]
            [lang.parser.reference :as reference]
            [lang.term :as term]))

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

(def ^:private access
  (parens
    (bind [_ (word ".")
           object (fwd expr)
           field (fwd expr)]
      (return {:ast/term :access
               :object   object
               :field    field}))))

(def ^:private application
  (parens
    (bind [function (fwd expr)
           arguments (many (fwd expr))]
      (return {:ast/term  :application
               :function  function
               :arguments arguments}))))

(def ^:private variant
  (brackets
    (bind [injector reference/keyword
           value (optional (fwd expr))]
      (return {:ast/term :variant
               :variant  {:injector injector :value value}}))))

(def ^:private atom
  (bind [atom atom/expr]
    (return {:ast/term :atom
             :atom     atom})))

(def ^:private symbol
  (bind [symbol reference/variable]
    (return {:ast/term :symbol
             :symbol   symbol})))

(def ^:private quote
  (letfn [(wrap [form]
            (if (and (term/is? form) (not (-> form :ast/term #{:unquote :unquote-splicing})))
              {:ast/term :quote
               :body     (->> form
                           (map (fn [[k v]] [k (wrap v)]))
                           (into {}))}
              form))]
    (bind [_ (sym \`)
           body (fwd expr)]
      (return (wrap body)))))

(def ^:private unquote
  (bind [_ (sym \,)
         body (fwd expr)]
    (return {:ast/term :unquote
             :body     body})))

(def ^:private unquote-splicing
  (bind [_ (word ",@")
         body (fwd expr)]
    (return {:ast/term :unquote-splicing
             :body      body})))

(def expr
  (<|>
    (<:> lambda)
    (<:> match)
    (<:> access)
    application
    variant
    atom
    symbol
    quote
    (<:> unquote)
    unquote-splicing))

(comment

  (blancas.kern.core/run*
    expr
    "`(match ,test true ,tru false ,fls)")


  )
