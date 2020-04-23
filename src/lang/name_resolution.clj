(ns lang.name-resolution
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :as walk]
            [lang.utils :refer [undefined]]))

(defn- resolve-name
  [imports node]
  (let [modules
        (->> imports
          (map (juxt :alias :module))
          (into {}))]
    (match node
      {:reference _ :in (source :guard (partial contains? modules))}
      (assoc node :in (get modules source))

      _ node)))

(defn run
  [module]
  (let [definitions* (walk/prewalk
                       (partial resolve-name (:imports module))
                       (:definitions module))]
    (assoc module :definitions definitions*)))
