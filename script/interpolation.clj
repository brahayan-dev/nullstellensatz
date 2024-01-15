#!/usr/bin/env bb

(ns interpolation)

(def ^:private data
  {:x [100.0
       200.0
       300.0
       400.0
       500.0]
   :y [0.033349521
       0.291597896
       1.292707
       3.893564042
       9.418162896]})

(def ^:private indexes [0 1 2])

(defn- ->selected-items [data]
  (let [->x-item #(get-in data [:x %])
        ->y-item #(get-in data [:y %])]
    (mapv #(vector (->x-item %) (->y-item %)) indexes)))

(defn- ->calculated-values [items]
  items)

(defn- ->answer []
  (-> data ->selected-items ->calculated-values))

(->answer)
