(ns lang.jvm)

(def primitives
  (->>
    {:bool    Boolean/TYPE
     :boolean Boolean
     :int     Integer/TYPE
     :integer BigInteger
     :object  Object
     :string  String
     :unit    Void/TYPE}
    (map (fn [[k v]] [{:ast/type :primitive :primitive k} v]))
    (into {})))

(defn native?
  [symbol]
  (->> symbol :in :name (first) (= "java")))

(defn primitive?
  [class]
  (contains? #{Integer/TYPE Void/TYPE Boolean/TYPE Float/TYPE Double/TYPE Long/TYPE Character/TYPE Byte/TYPE Short/TYPE} class))

(defn subclass?
  [sub super]
  (cond
    (and (class? sub) (class? super))
    (contains?
      (conj (set (supers sub)) sub)
      super)

    (and (vector? sub) (vector? super))
    (and (= (count sub) (count super))
         (every?
           (partial apply subclass?)
           (zipmap
             super
             sub)))))
