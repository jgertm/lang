(ns user
  (:require [com.gfredericks.debug-repl]))

(defn before-refresh []
  (try
    (com.gfredericks.debug-repl/unbreak!!)
    (catch Exception e nil)))

(defn after-refresh []
  nil
  #_(do (println "\n–-—")
        (-main "signature"
          ;; "std/lang/option.lang"
          ;; "std/lang/list.lang"
          ;; "std/lang/math.lang"
          ;; "std/lang/core.lang"
          ;; "examples/option.lang"
          ;; "examples/linked-list.lang"
          ;; "examples/arithmetic.lang"
          ;; "std/lang/io.lang"
          "std/lang/core.lang")))
