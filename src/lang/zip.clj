(ns lang.zip
  (:refer-clojure :exclude [empty replace])
  (:require [clojure.zip :as zip]))

(declare insert-left)
(declare insert-right)

(defonce ^:private start-element
  (gensym "start"))

(defonce ^:private end-element
  (gensym "end"))

(def empty
  [end-element {:left [start-element]}])

(defn node
  [[current _]]
  (when-not (#{start-element end-element} current)
    current))

(defn start?
  [[current state]]
  (= current start-element))

(defn end?
  [[current state]]
  (= current end-element))

(defn left
  [[current state :as zipper]]
  (if (and (some? current) (not (start? zipper)))
    [(peek (:left state))
     (-> state
       (update :left pop)
       (update :right (fnil conj '()) current))]
    zipper))

(defn right
  [[current state :as zipper]]
  (if (and (some? current) (not (end? zipper)))
    [(peek (:right state))
     (-> state
       (update :left (fnil conj []) current)
       (update :right pop))]
    zipper))

(defn ->start
  [[current state :as zipper]]
  (if (start? zipper)
    zipper
    [(first (:left state))
     (-> state
       (dissoc :left)
       (assoc :right (concat (rest (:left state)) [current] (:right state))))]))

(defn ->end
  [[current state :as zipper]]
  (if (end? zipper)
    zipper
    [(last (:right state))
     (-> state
       (dissoc :right)
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
  [[current state :as zipper] element]
  (cond
    (start? zipper)
    (insert-right zipper element)

    (some? current)
    [current (update state :left (fnil conj []) element)]

    :default
    [element state]))

(defn insert-right
  [[current state :as zipper] element]
  (cond
    (end? zipper)
    (insert-left zipper element)
    
    (some? current)
    [current (update state :right (fnil conj '()) element)]

    :default
    [element state]))

(defn focus-left
  ([zipper target]
   (focus-left zipper target =))
  ([[current _ :as zipper] target eq-fn]
   (if-not (some #(eq-fn % target) (left-seq zipper))
     (throw (Exception. "target not found"))
     (if (or (eq-fn current target) (start? zipper))
       zipper
       (focus-left (left zipper) target)))))

(defn focus-right
  ([zipper target]
   (focus-right zipper target =))
  ([[current _ :as zipper] target eq-fn]
   (if (or (eq-fn current target) (end? zipper))
     zipper
     (focus-right (right zipper) target))))

(defn truncate-left
  [[current state]]
  [current (dissoc state :left)])

(defn truncate-right
  [[current state]]
  [current (dissoc state :right)])
