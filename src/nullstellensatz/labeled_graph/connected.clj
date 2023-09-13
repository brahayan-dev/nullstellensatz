(ns nullstellensatz.labeled-graph.connected
  (:require [clojure.math.combinatorics :as math]))

(defrecord Pack [k-value
                 p-k-value
                 bin-value
                 factor-value])

(defn- ->subset-value [k] ((Math/pow 2 k) 1))
(defn- ->binomial-value [k p] (-> p range (math/count-combinations k)))

(defn- ->packs [k-values p]
  (let [->pack (fn [k] (Pack. k
                             (- p k)
                             (->subset-value k)
                             (->binomial-value k p)))]
  (map ->pack k-values)))

(defn- generate-packs [p]
  (->> p (range 1) (->packs p)))

(defn ->size [p]
  (let [->product #(-> % vals (apply *'))]
    (->> p generate-packs (map ->product) (apply +'))))
