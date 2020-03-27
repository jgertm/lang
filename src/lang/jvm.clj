(ns lang.jvm)

(defn native?
  [symbol]
  (->> symbol :in :name (first) (= "java")))
