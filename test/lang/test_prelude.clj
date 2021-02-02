(ns lang.test-prelude
  (:require [clojure.string :as str]
            [lang.compiler :as compiler]
            matcher-combinators.midje
            midje.repl
            potemkin)
  (:import java.io.StringReader))

(potemkin/import-vars
  [midje.sweet
   fact facts
   provided
   contains just exactly
   with-state-changes before after around]
  [midje.repl autotest]
  [matcher-combinators.matchers
   equals embeds])

(def matches matcher-combinators.midje/match)

(defmacro run
  [phase & fs]
  `(binding [*print-namespace-maps* false]
       (-> (str/join " " (map pr-str (quote ~fs)))
         (str/replace #"[,]" "")
         (StringReader.)
         (compiler/run :until ~phase))))

