(ns nullstellensatz.labeled-connected-graph
  (:require
   [clojure.math :refer [pow]]
   [clojure.set :refer [union]]
   [nullstellensatz.combination :as combination]
   [nullstellensatz.labeled-connected-graph :as connected]
   [nullstellensatz.subset :as subset]))

(defn- count-nodes [k] ((comp int dec pow) 2 k))

(defn- ->term [n k cache]
  (let [first-val (get cache k)
        label-val (count-nodes k)
        second-val (get cache (-' n k))
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
      (if (< r value) {:r r :k k :n n :p p :cache updated-cache}
          (recur (inc k) (- r value) updated-cache)))))

(defn- ->tag [{:keys [r p k cache] :as answer}]
  (let [a (get cache k)
        b (get cache p)
        c (count-nodes k)]
    (assoc answer
           :r (rem r (* a b c))
           :t (quot r (* a b c)))))

(defn- ->node [{:keys [k p r cache] :as answer}]
  (let [a (get cache k)
        b (get cache p)]
    (assoc answer
           :r (rem r (* a b))
           :v (quot r (* a b)))))

(defn- ->element [{:keys [n k t v r p cache]}]
  (let [a (get cache p)]
    [n k t v (quot r a) (rem r a)]))

(defn unrank [n r]
  (case n
    0 [0 0 0 0 0 0]
    1 [1 1 0 0 0 0]
    ((comp ->element ->node ->tag ->location) n r)))

;; TODO: can it be optimized?
;;       can it be enhanced with atomic DP?
(defn unwrap [n r cache]
  (let [[_ k _ _ p q :as code] (unrank n r)
        updated-cache (conj cache code)]
    (if (#{0 1} n) (apply sorted-set updated-cache)
        (union
         (unwrap k p updated-cache)
         (unwrap (- n k) q updated-cache)))))

#_(defn- ->graph-tags [n items]
    (let [first-tags (->> items (map inc) (cons 1))
          second-tags (remove #(some #{%} first-tags) (range 1 (inc n)))]
      (println {:i items :n n :t1 first-tags :t2 second-tags})
      (map vec [first-tags second-tags])))

#_(defn- ->labeled-graph [tags graph]
    (println {:g graph :t tags})
    (loop [[_ & g-tail] graph [a b & tail] tags answer []]
      (if (empty? g-tail) answer
          (recur g-tail tail
                 (cons [a b] answer)))))

#_(defn- ->connected-graphs [i nodes first-graph second-graph]
    (let [new-graph (map #(vector i %) nodes)]
      ((comp vec concat) first-graph second-graph new-graph)))

#_(defn- load-graph [cache [n k t v]]
    (let [p (- n k)
          n_ (- n 2)
          k_ (dec k)
          v_ (+ v 2)
          [first-tags second-tags] (->> t inc (combination/generate n_ k_) (->graph-tags n))
          first-graph (->> k (get cache) (->labeled-graph first-tags))
          second-graph (->> p (get cache) (->labeled-graph second-tags))
          nodes (->> v_ (subset/generate n) (cons 1) vec)
          item (last first-tags)]
      (->connected-graphs item nodes first-graph second-graph)))

(defn compact [cache [n k t v]]
  (let [n_ (- n 2)
        k_ (dec k)
        first-graph (get cache k)
        second-graph (get cache (- n k))
        nodes (->> v (+ 2) (subset/generate n) vec)
        tags (->> t inc (combination/generate n_ k_) (map inc) (cons 1) vec)]
    (vector n k tags nodes first-graph second-graph)))

(defn- ->graph [cache item]
  (let [n (get item 0)
        object (case n
                 1 [[1]]
                 2 [[1 2]]
                 ((comp compact) cache item))]
    (assoc cache n object)))

(defn- debug-log
  ([x]
   (debug-log "X" x))
  ([title x]
   (let [t (str "|---|" title "|-------------|")]
     (clojure.pprint/pprint t)
     (clojure.pprint/pprint x)
     (clojure.pprint/pprint t) x)))

(defn generate [n r]
  (let [codes (unwrap n r #{})]
    (as-> codes $ (reduce ->graph {} $) (debug-log $) (get $ n))))

(comment
  (generate 3 3))
