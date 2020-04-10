(ns lang.interpreter
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :as walk]
            [lang.utils :refer [undefined]]))

(defn- lookup-binding ; TODO: include globals
  [environment symbol]
  (or
    (get environment symbol)
    (throw (ex-info "Unknown binding" {:symbol symbol}))))

(defn run
  [environment term]
  (match term
    {:ast/term :quote :body body}
    (walk/prewalk
      (fn [term]
        (match term
          {:ast/term :unquote}
          (run environment (:body term))

          _ term))
      body)

    {:ast/term :symbol :symbol symbol}
    (lookup-binding environment symbol)))
