(ns nullstellensatz.object.irreducible-linked-diagram
  (:require [nullstellensatz.common :as common]
            [schema.core :as s]))

(defn- ->term [n k cache]
  (let [first-val (get cache k)
        index-val (dec (*' k 2))
        second-val (get cache (-' n k))]
    (*' index-val first-val second-val)))

(defn- ->updated-cache [i cache]
  (loop [k 1 acc 0]
    (if (> k (dec i))
      (assoc! cache i acc)
      (let [value (->term i k cache)]
        (recur (inc k) (+' acc value))))))

(defn- enumerate*
  "Number of irreducible linked diagrams on n arcs, via the recurrence
  I(n) = sum_{k=1}^{n-1} (2k-1) * I(k) * I(n-k)."
  [n]
  (cond
    (<= n 0) 0
    (#{1 2} n) 1
    :else (loop [i 3 cache (transient {1 1 2 1})]
            (if (> i n) (get (persistent! cache) n)
                (recur (inc i) (->updated-cache i cache))))))

(def enumerate (memoize enumerate*))

(defn- ->location [n r]
  (loop [k 1 r r]
    (let [p (-' n k)
          index-val (dec (*' k 2))
          first-val (enumerate k)
          second-val (enumerate p)
          value (*' index-val first-val second-val)]
      (if (< r value) {:r r :k k :n n :p p}
          (recur (inc k) (-' r value))))))

(defn- ->slot [{:keys [r p k] :as answer}]
  (let [a (enumerate k)
        b (enumerate p)]
    (assoc answer
           :r (rem r (*' a b))
           :j (quot r (*' a b)))))

(defn- ->element [{:keys [n k j p r]}]
  (let [v (enumerate p)
        a (quot r v)
        b (rem r v)]
    (vector n k j a b)))

(defn unrank [n m]
  (case n
    1 [1 1 0 0 0]
    2 [2 1 0 0 0]
    ((comp ->element ->slot ->location) n m)))

;; FIXME: What happens when k > 2|x| - 2?
(defn concat-codes [k_ x-code y-code]
  (let [k (inc k_)
        x-size (*' 2 (count x-code))
        y-size (dec (*' 2 (count y-code)))
        full-size (+' x-size y-size 1)
        ->first-part (fn [[a b]]
                       (cond
                         (and (> a k) (> b k)) [(+' a y-size) (+' b y-size)]
                         (> a k) [(+' a y-size) b]
                         (> b k) [a (+' b y-size)]
                         :else [a b]))
        ->second-part (fn [[a b]]
                        (cond
                          (= (+ b x-size) full-size) [(+' a k) (+' b x-size)]
                          :else [(+' a k) (+' b k)]))]
    (concat (map ->first-part x-code) (map ->second-part y-code))))

(defn generate [n m]
  (let [[_ k j a b] (unrank n m)
        p (-' n k)]
    (if (= n 1)
      [[1 2]]
      (concat-codes j
                    (generate k a)
                    (generate p b)))))

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
