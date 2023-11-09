(ns nullstellensatz.noncrossing-linked-diagram
  (:require [nullstellensatz.common :as common]))

(defn- enumerate-worker [n cache]
  (let [k-vals (common/->ints-from-zero-to-val-less-one n)
        ->terms #(* (get cache %) (get cache (- n 1 %)))]
    (if (< n 2) 1
        (->> k-vals (map ->terms) (apply +')))))

(defn- enumerate-helper [n k-vals cache]
  (loop [[head & tail :as k-vals] k-vals cache cache]
    (if (empty? k-vals) (get cache n)
        (let [updated-val (enumerate-worker head cache)
              updated-cache (assoc cache head updated-val)]
          (recur tail updated-cache)))))

(defn enumerate [n]
  (let [k-vals (common/->ints-from-zero-to-val n)]
    (enumerate-helper n k-vals {})))

