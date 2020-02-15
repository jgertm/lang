(ns lang.zip
  (:refer-clojure :exclude [empty replace])
  (:require [clojure.zip :as zip]))

(declare insert-left)
(declare insert-right)

(defonce ^:private start-element
  (gensym "start"))

(defonce ^:private end-element
  (gensym "end"))

(defn left-f
  [f]
  (fnil f [start-element]))

(defn right-f
  [f]
  (fnil f `(~end-element)))

(def empty
  [end-element {:left [start-element] :right '()}])

(defn make
  [coll]
  [(last coll) {:left (vec (butlast coll))}])

(defn node
  [[current _]]
  current)

(defn start?
  [[current state]]
  (= current start-element))

(defn end?
  [[current state]]
  (= current end-element))

(defn left
  [[current state :as zipper]]
  (if-not (start? zipper)
    [(peek (:left state))
     (-> state
       (update :left pop)
       (update :right (right-f conj) current))]
    zipper))

(defn right
  [[current state :as zipper]]
  (if (not (end? zipper))
    [(peek (:right state))
     (-> state
       (update :left (left-f conj) current)
       (update :right pop))]
    zipper))

(defn ->start
  [[current state :as zipper]]
  (if (start? zipper)
    zipper
    [(first (:left state))
     (-> state
       (assoc :left [])
       (assoc :right (concat (rest (:left state)) [current] (:right state))))]))

(defn ->end
  [[current state :as zipper]]
  (if (end? zipper)
    zipper
    [(last (:right state))
     (-> state
       (assoc :right '())
       (assoc :left (vec (concat (:left state) [current] (butlast (:right state))))))]))

(defn left-seq
  [[current state]]
  (cons current (rseq (:left state))))

(defn right-seq
  [[current state]]
  (cons current (:right state)))

(defn replace
  [[current state :as zipper] replacement]
  (cond
    (start? zipper)
    (insert-right zipper replacement)
    
    (end? zipper)
    (insert-left zipper replacement)

    :default
    [replacement state]))

(defn edit
  [[current state] f]
  [(f current) state])

(defn insert-left
  [[current state :as zipper] & elements]
  (cond
    (start? zipper)
    (apply insert-right zipper elements)

    (some? current)
    [current (update state :left (left-f into) elements)]

    :default
    [(last elements)
     (update state :left (left-f into) (reverse (butlast elements)))]))

(defn insert-right
  [[current state :as zipper] & elements]
  (cond
    (end? zipper)
    (apply insert-left zipper elements)
    
    (some? current)
    [current (update state :right (right-f into) (reverse elements))]

    :default
    [(last elements)
     (update state :right (right-f into) (reverse (butlast elements)))]))

(defn focus-left
  ([zipper target]
   (focus-left zipper target =))
  ([[current _ :as zipper] target eq-fn]
   (cond
     (start? zipper)
     (throw (Exception. "target not found"))

     (eq-fn current target)
     zipper

     :else
     (focus-left (left zipper) target eq-fn))))

(defn focus-right
  ([zipper target]
   (focus-right zipper target =))
  ([[current _ :as zipper] target eq-fn]
   (if (or (eq-fn current target) (end? zipper))
     zipper
     (focus-right (right zipper) target))))

(defn split
  [[current state]]
  [[current (assoc state :right `(~end-element))]
   (butlast (:right state))])
