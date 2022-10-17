(ns lang.core
  (:gen-class)
  (:require [lang.compiler :as compiler]
            [lang.module :as module]
            [taoensso.timbre :as log]))

(log/set-level! :trace)

(defn -main
  [& args]
  ;; TODO: use CLI args parser
  (compiler/init)
  (run!
    (case (first args)
      "signature"
      #(-> %
         (compiler/run :until :type-checker)
         (module/signature)
         (println))

      "compile"
      compiler/run)
    (next args)))
