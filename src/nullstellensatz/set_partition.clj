(ns nullstellensatz.set-partition
  (:require
   [nullstellensatz.combination :as combination]))

(defn- ->ints-from-zero-to-val [n]
  (range 0 n))

(defn ->binomial [n k]
  (combination/enumerate (dec n) k))

(defn- enumerate-worker [n answer]
  (if (< n 2) 1
      (->> n
           ->ints-from-zero-to-val
           (map #(->binomial n %))
           (map *' answer)
           (apply +'))))

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

(defn- find-term [n index position]
  (let [binomial-val (->binomial n position)
        aux-val (* binomial-val (enumerate position))]
    (if (>= aux-val index) [position index]
        (recur n (- index aux-val) (inc position)))))

(defn encode [n index]
  (let [[k-val tail] (find-term n index 0)
        bell-val (enumerate k-val)
        binomial-val (quot tail bell-val)
        previous-bell-val (rem tail bell-val)]
    [n k-val binomial-val previous-bell-val]))




