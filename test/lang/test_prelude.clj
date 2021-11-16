(ns lang.test-prelude
  (:require [clojure.string :as str]
            [lang.compiler :as compiler]
            potemkin)
  (:import java.io.StringReader))

(defmacro run
  [phase & fs]
  (binding [*print-namespace-maps* false]
    (let [program
          (mapv
           (fn normalize-form [f]
             (if (string? f)
               f
               (pr-str f)))
           fs)]
      `(-> (str/join " " ~program)
           (str/replace #"[,]" "")
           (StringReader.)
           (compiler/run :until ~phase)))))
