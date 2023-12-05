(ns nullstellensatz.labeled-connected-graph
  (:require
   [clojure.math :refer [pow]]
   [nullstellensatz.combination :as combination]))

(defn- ->updated-cache [i cache]
  (loop [k 1 answer []]
    (if (> k (dec i))
      (assoc cache i (apply +' answer))
      (let [first-val (get cache k)
            second-val (get cache (-' i k))
            label-val ((comp int dec pow) 2 k)
            binomial-val (combination/enumerate (-' i 2) (dec k))
            result-val (*' binomial-val label-val first-val second-val)]
        (recur (inc k) (cons result-val answer))))))

(defn enumerate [n]
  (loop [i 2 cache {1 1}]
    (if (> i n) (get cache n)
        (recur (inc i) (->updated-cache i cache)))))

(defn ->k-val [n r]
  (loop [r r k-val nil answer [n]]
    (if (-> k-val nil? not) {:result r
                             :answer (cons k-val answer)}
        (let [jump? (nil? k-val)
              k (if jump? r (-' r value))]
          (recur k
                 (>= k value)
                 (if jump? (cons k answer) answer))))))
