(ns nullstellensatz.labeled-graph.connected
  (:require [clojure.math.combinatorics :refer [count-combinations]]))

(defrecord BasicPack [k-value
                      p-k-value
                      subset-value
                      binomial-value])

(defrecord NumericPack [k-quantity
                        p-k-quantity
                        subset-value
                        binomial-value])

(defn- ->subset-value [k] (->> k (Math/pow 2) dec int))

(defn- ->binomial-value [p k]
  (let [->combinations #(->> k dec (count-combinations %))]
    (-> p (- 2) range ->combinations)))

(defn- ->packs [k-values p]
  (let [->pack (fn [k]
                 (BasicPack. k
                             (- p k)
                             (->subset-value k)
                             (->binomial-value p k)))]
    (map ->pack k-values)))

(defn- ->basic-packs [p]
  (let [->values #(if (= 1 %) '(1) (range 1 %))]
    (-> p ->values (->packs p))))

(defn- ->g-quantity [n _packs]
  (let [->treat #(%)
        ->cache (memoize #(cond
                            (< % 3) 1
                            :else (->treat %)))]
    (->cache n)))

(defn- ->numeric-packs [packs]
  (let [->translated #(NumericPack. (-> % :k-value (->g-quantity packs))
                                    (-> % :p-k-value (->g-quantity packs))
                                    (:subset-value %) (:binomial-value %))]
    (map ->translated packs)))

(defn ->size [p]
  (let [->product #(->> % vals (apply *'))]
    (->> p ->basic-packs ->numeric-packs (map ->product) (apply +'))))

(comment (let [p 1]
           (clojure.pprint/pprint {:size (->size p)
                                   :basic-packs (->basic-packs p)
                                   :g-quantity (->g-quantity 0 [])
                                   :numeric-packs ((comp ->numeric-packs ->basic-packs) p)})))
