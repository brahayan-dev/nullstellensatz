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

(defn- ->values [n i k]
  (let [term (* (enumerate k) (enumerate (- n k 1)))]
    (if (>= term i) [k i]
        (recur n (- i term) (inc k)))))

(defn encode [n i]
  (if (some zero? [n i])
    [n 0 0 0]
    (let [[k m] (->values n i 0)
          catalan-val (enumerate (- n k 1))
          p (quot m catalan-val)
          q (rem m catalan-val)]
      [n k p q])))

(defn search-by-code-helper [n k p q answer]
  (if (zero? n) []
      (let [n_ (dec n)]
        (recur n_ k p q answer))))

(defn search-by-code [[n k p q]]
  (search-by-code-helper n k p q []))

(def search
  (comp search-by-code encode))
