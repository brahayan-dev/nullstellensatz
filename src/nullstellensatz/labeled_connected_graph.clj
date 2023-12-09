(ns nullstellensatz.labeled-connected-graph
  (:require
   [clojure.math :refer [pow]]
   [nullstellensatz.combination :as combination]
   [nullstellensatz.labeled-connected-graph :as connected]))

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

(defn unwrap [n r]
  (loop [cache {[n r] (unrank n r)}]
    (let [[m k _ _ a b] (get cache [n r])
          can-push? #(not (contains? cache %))
          p (- m k)]
      (if (>= 1 m)
        (assoc cache [m 0] [m 0 0 0 0 0])
        (recur (cond-> cache
                 (can-push? [k a]) (assoc [k a] (unrank k a))
                 (can-push? [p b]) (assoc [p b] (unrank p b))))))))

(comment
  (unwrap 4 37))
