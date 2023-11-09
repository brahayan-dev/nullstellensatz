(ns nullstellensatz.set-partition
  (:require
    [nullstellensatz.common :as common]
   [nullstellensatz.combination :as combination]))


(defn ->binomial [n k]
  (combination/enumerate (dec n) k))

(defn ->binomial-object [n k index]
  (combination/generate (dec n) k (inc index)))

(defn- enumerate-worker [n answer]
  (if (< n 2) 1
      (->> n
           common/->ints-from-zero-to-val-less-one
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
  (let [k-vals (common/->ints-from-zero-to-val-less-one n)]
    ((comp first enumerate-helper) n k-vals [])))

(defn- find-term [n index position]
  (let [binomial-val (->binomial n position)
        term-val (* binomial-val (enumerate position))]
    (if (> term-val index) [position index]
        (recur n (- index term-val) (inc position)))))

(defn encode [n index]
  (let [[k-val tail] (find-term n index 0)
        bell-val (enumerate k-val)
        binomial-val (quot tail bell-val)
        previous-bell-val (rem tail bell-val)]
    [n k-val binomial-val previous-bell-val]))

(defn wrap [n i]
  (loop [n n index i answer []]
    (let [[_ k-val _ previous-bell-val :as code] (encode n index)
          updated-answer (cons code answer)]
      (if (< n 2) updated-answer
          (recur k-val previous-bell-val updated-answer)))))

(defn- update-block [binomial]
  (let [->updated-block #(get binomial (dec %) %)]
    (fn [block] (mapv ->updated-block block))))

(defn exist? [v item]
  ((comp not nil? some) #{item} v))

(defn- ->object [previous-partition [n k-val binomial-val _]]
  (case n
    0 previous-partition
    1 (cons [1] previous-partition)
    (let [binomial (->binomial-object n k-val binomial-val)
          block (remove #(exist? binomial %) (common/->ints-from-one-to-val n))]
      (-> binomial update-block (map previous-partition) (conj block)))))

(defn search-by-codes [wrapping]
  (reduce ->object [] wrapping))

(def search (comp search-by-codes wrap))

