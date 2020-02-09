(ns lang.parser.lexer
  (:require [blancas.kern.core :refer :all]
            [blancas.kern.lexer :as lexer]))

(def ^:private identifier-start
  (<|> letter (one-of* "*&^%$!_-+=<>?/")))

(def ^:private identifier-next
  (<|> alpha-num (one-of* "*&^%$!_-+=<>?")))

(let [parsers
      (lexer/make-parsers
        (merge lexer/basic-def
          {:comment-line      ";"
           :identifier-start  identifier-start
           :identifier-letter identifier-next}))]
  (def trim       (:trim       parsers))
  (def lexeme     (:lexeme     parsers))
  (def sym        (:sym        parsers))
  (def new-line   (:new-line   parsers))
  (def one-of     (:one-of     parsers))
  (def none-of    (:none-of    parsers))
  (def token      (:token      parsers))
  (def word       (:word       parsers))
  ;; (def identifier (:identifier parsers))
  (def field      (:field      parsers))
  (def char-lit   (:char-lit   parsers))
  (def string-lit (:string-lit parsers))
  (def dec-lit    (:dec-lit    parsers))
  (def oct-lit    (:oct-lit    parsers))
  (def hex-lit    (:hex-lit    parsers))
  (def float-lit  (:float-lit  parsers))
  (def bool-lit   (:bool-lit   parsers))
  (def nil-lit    (:nil-lit    parsers))
  (def parens     (:parens     parsers))
  (def braces     (:braces     parsers))
  (def angles     (:angles     parsers))
  (def brackets   (:brackets   parsers))
  (def semi       (:semi       parsers))
  (def comma      (:comma      parsers))
  (def colon      (:colon      parsers))
  (def dot        (:dot        parsers))
  (def semi-sep   (:semi-sep   parsers))
  (def semi-sep1  (:semi-sep1  parsers))
  (def comma-sep  (:comma-sep  parsers))
  (def comma-sep1 (:comma-sep1 parsers)))

(def identifier
  (<+> identifier-start (many0 identifier-next)))
