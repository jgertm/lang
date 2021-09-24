(ns lang.test-prelude
  (:require [clojure.string :as str]
            [lang.compiler :as compiler]
            potemkin)
  (:import java.io.StringReader))

(defmacro run
  [phase & fs]
  `(binding [*print-namespace-maps* false]
       (-> (str/join " " (map pr-str (quote ~fs)))
         (str/replace #"[,]" "")
         (StringReader.)
         (compiler/run :until ~phase))))

