(ns lang.typeclass2
  (:refer-clojure :exclude [type ==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.pldb :as pldb]))

(pldb/db-rel type t)
(pldb/db-rel typeclass tc)
(pldb/db-rel instance tc t c)

(def db
  (pldb/db
   [type 'String]
   [type 'Integer]
   [type 'Function]
   [type `(~'Option ~(lvar))]

   [typeclass 'Show]
   [typeclass 'Eq]

   (let [v (lvar)]
     [instance 'Show `(~'Option ~v)
      [`(~'Show ~v) `(~'Eq ~v)]])
   (let [v (lvar)]
     [instance 'Eq `(~'Option ~v)
      [`(~'Eq ~v)]])
   [instance 'Show 'Integer []]
   [instance 'Show 'String []]
   [instance 'Eq 'Integer []]
   ;; [instance 'Eq 'String []]

))

db

(declare instanceo)

(defna subinstanceso
  [scs i]
  ([[[stc st] . mscs] _]
   (fresh [sih sit]
     (instanceo stc st sih)
     (subinstanceso mscs sit)
     (conso sih sit i)))

  ([[] _]
   (== i nil)))

(defn instanceo
  [tc t i]
  (fresh [scs ih it]
    (type t)
    (typeclass tc)
    (instance tc t scs)
    (subinstanceso scs it)
    (== ih [tc t])
    (conso ih it i)))

(run-db* db [q]
         (fresh [t]
           (instanceo 'Eq '(Option Integer) q)))

(run-db* db [q]
         (fresh [t]
           (instanceo 'Eq '(Option (Option Integer)) q))) ; doesnt work
