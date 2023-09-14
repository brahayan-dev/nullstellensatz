(ns nullstellensatz.labeled-graph.connected
  (:require [clojure.math.combinatorics :as math]))

(defrecord BasicPack [k-value
                      p-k-value
                      subset-value
                      binomial-value])

(defn- ->subset-value [k] (->> k (Math/pow 2) dec int))

(defn- ->binomial-value [p k]
  (let [->combinations #(->> k dec (math/count-combinations %))]
    (-> p (- 2) range ->combinations)))

(defn- ->packs [k-values p]
  (let [->pack (fn [k]
                 (BasicPack. k
                             (- p k)
                             (->subset-value k)
                             (->binomial-value p k)))]
  (map ->pack k-values)))

(defn- generate-packs [p]
  (let [->values #(if (= 1 %) '(1) (range 1 %))]
    (-> p ->values (->packs p))))

(defn ->size [p]
  (let [->product #(->> % vals (apply *'))]
    (->> p generate-packs (map ->product) (apply +'))))

(->size 3)
(generate-packs 4)
