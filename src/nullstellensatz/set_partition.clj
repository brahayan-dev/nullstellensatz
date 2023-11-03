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

;; def search_partition_with_code(code):
;;     n,k,m1,m2 = code
;;     if n==0:
;;         return []
;;     else:
;;         relabel_set = gen_combination(n-1,k,m1)
;;         partial_partition = search_partition_with_index(k,m2)
;;         partial_partition = [[relabel_set[i-1] for i in block] for block in partial_partition]
;;         last_block = [i for i in range(1,n+1) if i not in relabel_set]
;;         partial_partition.append(last_block)
;;     return partial_partition

        
(defn search-by-code [[n k-val binomial-val previous-bell-val]]
  nil)

(def search (comp search-by-code encode))


