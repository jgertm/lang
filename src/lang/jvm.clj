(ns lang.jvm)

(def primitives
  (->>
    {:unit    'void
     :string  'java.lang.String
     :integer 'java.math.BigInteger
     :int     'int
     :boolean 'java.lang.Boolean
     :object  'java.lang.Object}
    (map (fn [[k v]] [{:ast/type :primitive :primitive k} v]))
    (into {})))

(defn native?
  [symbol]
  (->> symbol :in :name (first) (= "java")))
