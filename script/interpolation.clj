#!/usr/bin/env bb

(ns interpolation)

(def ^:private data
  {:x [1
       100.0
       200.0
       300.0
       400.0
       500.0]
   :y [0.003085083
       0.033349521
       0.291597896
       1.292707
       3.893564042
       9.418162896]})

(def ^:private indexes [0 1 4])

(defn- ->selected-items [data]
  (let [->x-item #(get-in data [:x %])
        ->y-item #(get-in data [:y %])]
    (mapv #(vector (->x-item %) (->y-item %)) indexes)))

(defn- ->calculated-b-values [x0 y0 x1 y1 x2 y2]
  (let [b0 y0
        b1 (/ (- y1 y0) (- x1 x0))
        bx (/ (- y2 y1) (- x2 x1))
        b2 (/ (- bx b1) (- x2 x0))]
    (vector b0 b1 b2)))

(defn- ->calculated-a-values [b0 b1 b2 x0 x1]
  (let [a0 (- b0 (* b1 x0) (* -1 b2 x0 x1))
        a1 (- b1 (* b2 x0) (* b2 x1))
        a2 b2]
    (vector a0 a1 a2)))

(defn- ->formatted-model [a0 a1 a2]
  (str a0 " + " a1 " * x " " + " a2 " * (x ** 2)"))

(defn- ->calculated-model [[[x0 y0] [x1 y1] [x2 y2]]]
  (let [[b0 b1 b2] (->calculated-b-values x0 y0 x1 y1 x2 y2)
        [a0 a1 a2] (->calculated-a-values b0 b1 b2 x0 x1)]
    (->formatted-model a0 a1 a2)))

(defn- ->answer []
  (-> data ->selected-items ->calculated-model println))

(->answer)
