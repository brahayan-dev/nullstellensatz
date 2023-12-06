(ns nullstellensatz.labeled-connected-graph
  (:require
   [clojure.math :refer [pow]]
   [nullstellensatz.combination :as combination]
   [nullstellensatz.labeled-connected-graph :as connected]))

(defn- ->term [n k cache]
  (let [first-val (get cache k)
        second-val (get cache (-' n k))
        label-val ((comp int dec pow) 2 k)
        binomial-val (combination/enumerate (-' n 2) (dec k))]
    (*' binomial-val label-val first-val second-val)))

(defn- ->updated-cache [i cache]
  (loop [k 1 answer []]
    (if (> k (dec i))
      (assoc cache i (apply +' answer))
      (let [value (->term i k cache)]
        (recur (inc k) (cons value answer))))))

(defn enumerate [n]
  (loop [i 2 cache {1 1}]
    (if (> i n) (get cache n)
        (recur (inc i) (->updated-cache i cache)))))

(defn- ->location [n r]
  (loop [k 1 r r cache {1 1}]
    (let [p (- n k)
          can-update? #(not (contains? cache %))
          ->cache (fn [c i] (assoc c i (enumerate i)))
          updated-cache (cond-> cache
                          (can-update? k) (->cache k)
                          (can-update? p) (->cache p))
          value (->term n k updated-cache)]
      (if (< r value) {:r r :k k :n n :cache updated-cache}
          (recur (inc k) (- r value) updated-cache)))))

(defn- ->tag [{:keys [remainder answer]}]
  ())

(defn- ->node [{:keys [remainder answer]}]
  ())

(def unrank (comp ->node ->tag ->location))
