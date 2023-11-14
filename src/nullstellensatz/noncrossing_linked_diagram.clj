(ns nullstellensatz.noncrossing-linked-diagram
  (:require [nullstellensatz.common :as common]
            [nullstellensatz.combination :as combination]))

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

(defn count-mountain-ranges [n x y]
  (if (> (+ x y) (* 2 n)) 0
      (let [average (quot (+ x y) 2)
            dynamic-set (- (* 2 n) x)
            p (combination/enumerate dynamic-set (- n average))
            q (combination/enumerate dynamic-set (- n 1 average))]
        (- p q))))

(defn- unrank-helper [n i x y low answer]
  (if (= x ((comp inc *) n 2)) answer
      (let [m (count-mountain-ranges n x (inc y))
            jump? (<= i ((comp dec +) low m))]
        (recur n i
               (inc x)
               (if jump? (inc y) (dec y))
               (if jump? low (+ low m))
               (-> jump? (if 1 0) (cons answer))))))

(defn unrank [n i]
  (unrank-helper n i 1 0 0 []))

