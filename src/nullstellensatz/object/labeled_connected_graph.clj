(ns nullstellensatz.object.labeled-connected-graph
  (:require [nullstellensatz.common :as common]
            [schema.core :as s]
            [nullstellensatz.object.combination :as combination]
            [nullstellensatz.object.subset :as subset]))

(defn- count-nodes [k]
  (dec (reduce *' (repeat k 2))))

(defn- ->term
  [n k cache]
  (let [first-val (get cache k)
        label-val (count-nodes k)
        second-val (get cache (-' n k))
        binomial-val (combination/enumerate (-' n 2) (dec k))]
    (*' binomial-val label-val first-val second-val)))

(defn- ->updated-cache
  [i cache]
  (loop [k 1 acc 0]
    (if (> k (dec i))
      (assoc! cache i acc)
      (let [value (->term i k cache)]
        (recur (inc k) (+' acc value))))))

(defn- enumerate*
  "Number of labeled connected graphs on n nodes, via the recurrence
  C(n) = 2^binom(n-1,2) - sum_{k=1}^{n-1} binom(n-2,k-1) * (2^k - 1) * C(k) * C(n-k)."
  [n]
  (cond
    (<= n 0) 0
    (= n 1) 1
    :else (loop [i 2 cache (transient {1 1})]
            (if (> i n) (get (persistent! cache) n)
                (recur (inc i) (->updated-cache i cache))))))

(def enumerate (memoize enumerate*))

(defn- ->location [n r]
  (loop [k 1 r r]
    (let [p (-' n k)
          label-val (count-nodes k)
          first-val (enumerate k)
          second-val (enumerate p)
          binomial-val (combination/enumerate (-' n 2) (dec k))
          value (*' binomial-val label-val first-val second-val)]
      (if (< r value) {:r r :k k :n n :p p}
          (recur (inc k) (-' r value))))))

(defn- ->tag [{:keys [r p k] :as answer}]
  (let [a (enumerate k)
        b (enumerate p)
        c (count-nodes k)]
    (assoc answer
           :r (rem r (*' a b c))
           :t (quot r (*' a b c)))))

(defn- ->node [{:keys [k p r] :as answer}]
  (let [a (enumerate k)
        b (enumerate p)]
    (assoc answer
           :r (rem r (*' a b))
           :v (quot r (*' a b)))))

(defn- ->element [{:keys [n k t v r p]}]
  (let [a (enumerate p)]
    [n k t v (quot r a) (rem r a)]))

(defn unrank [n m]
  (case n
    0 [0 0 0 0 0 0]
    1 [1 1 0 0 0 0]
    ((comp ->element ->node ->tag ->location) n m)))

(def ^:private atomic-cache (atom {}))

(defn expand-codes [m s]
  (letfn [(clear-cache [answer] (reset! atomic-cache {}) answer)
          (->prepared [answer] (-> answer vals sort vec))
          (->answer  [n r]
            (if (contains? @atomic-cache [n r]) @atomic-cache
                (let [[_ k _ _ p q :as code] (unrank n r)
                      updated-cache (swap! atomic-cache assoc [n r] code)]
                  (if (#{0 1} n) updated-cache
                      (merge
                       (->answer k p)
                       (->answer (-' n k) q))))))]
    ((comp ->prepared clear-cache ->answer) m s)))

(defn- ->named-graph [graph tags]
  (loop [[head :as g] graph answer []]
    (if (empty? g) answer
        (let [->label #(->> head % dec (get tags))
              a (->label first)
              b (->label second)]
          (recur (rest g)
                 (conj answer [a b]))))))

(defn compact [cache [n k t v]]
  (let [n_ (-' n 2)
        k_ (dec k)
        first-graph (get cache k)
        second-graph (get cache (-' n k))
        nodes (->> v (+' 2) (subset/generate n) vec)
        tags (->> t inc (combination/generate n_ k_) (map inc) (cons 1) vec)]
    (vector n k tags nodes first-graph second-graph)))

(defn- count-vertices [nodes]
  (-> nodes flatten count))

(defn relabel [graph labels]
  (let [labels_ (vec labels)]
    (if (= 1 (count-vertices graph))
      (vector labels_)
      (->named-graph graph labels_))))

(defn- assemble [[n _ tags nodes g h]]
  (let [universe (range 1 (inc n))
        complements (remove #(some #{%} tags) universe)
        g_ (relabel g tags)
        h_ (relabel h complements)
        arcs (mapv #(vector % n) nodes)]
    (cond-> []
      (< 1 (count-vertices g_)) (concat g_)
      (< 1 (count-vertices h_)) (concat h_)
      true (concat arcs))))

;; TODO: verify a huge case within atomic-cache
(defn- ->graph [cache item]
  (let [n (get item 0)
        object (case n
                 1 [[1]]
                 2 [[1 2]]
                 ((comp vec assemble compact) cache item))]
    (assoc cache n object)))

(defn generate [n m]
  (let [codes (expand-codes n m)]
    (as-> codes $ (reduce ->graph {} $) (get $ n))))

(s/defschema EnumerateSchema
  {:n s/Int})

(def export-enumerate
  (common/->Export
   EnumerateSchema ::enumerate
   (fn [{:keys [n]}] (enumerate n))))

(s/defschema GenerateSchema
  {:n s/Int :m s/Int})

(def export-generate
  (common/->Export
   GenerateSchema ::generate
   (fn [{:keys [n m]}] (generate n m))))
