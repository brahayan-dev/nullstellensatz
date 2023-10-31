(ns nullstellensatz.set-partition
  (:require
   [nullstellensatz.combination :as combination]))

(defn- ->ints-from-zero-to-val [n]
  (range 0 n))

(defn- enumerate-worker [n answer]
  (let [->binomial #(combination/enumerate (dec n) %)]
    (if (< n 2) 1
        (->> n
             ->ints-from-zero-to-val
             (map ->binomial)
             (map *' answer)
             (apply +')))))

(defn- enumerate-helper [n k-vals answer]
  (if (empty? k-vals)
    (-> n (enumerate-worker answer) (cons answer))
    (let [k (first k-vals)
          tail (rest k-vals)
          updated-answer (-> k (enumerate-worker answer) (cons answer))]
      (recur n tail updated-answer))))

(defn enumerate [n]
  (let [k-vals (->ints-from-zero-to-val n)]
    (first (enumerate-helper n k-vals []))))
