(ns nullstellensatz.object.irreducible-linked-diagram
  (:require [nullstellensatz.common :as common]
            [schema.core :as s]))

(defn- ->term [n k cache]
  (let [first-val (get cache k)
        index-val (dec (*' k 2))
        second-val (get cache (-' n k))]
    (*' index-val first-val second-val)))

(defn ->updated-cache [i cache]
  (loop [k 1 answer []]
    (if (> k (dec i))
      (assoc cache i (apply +' answer))
      (let [value (->term i k cache)]
        (recur (inc k) (cons value answer))))))

(defn enumerate [n]
  (loop [i 3 cache {1 1 2 1}]
    (if (> i n) (get cache n)
        (recur (inc i) (->updated-cache i cache)))))

(defn- ->location [n r]
  (loop [k 1 r r cache {1 1}]
    (let [p (-' n k)
          can-update? #(not (contains? cache %))
          ->cache (fn [c i] (assoc c i (enumerate i)))
          updated-cache (cond-> cache
                          (can-update? k) (->cache k)
                          (can-update? p) (->cache p))
          value (->term n k updated-cache)]
      (if (< r value) {:r r :k k :n n :p p :cache updated-cache}
          (recur (inc k) (-' r value) updated-cache)))))

(defn- ->slot [{:keys [r p k cache] :as answer}]
  (let [a (get cache k)
        b (get cache p)]
    (assoc answer
           :r (rem r (*' a b))
           :j (quot r (*' a b)))))

(defn- ->element [{:keys [n k j p r cache]}]
  (let [v (get cache p)
        a (quot r v)
        b (rem r v)]
    (vector n k j a b)))

(defn unrank [n m]
  (case n
    1 [1 1 0 0 0]
    2 [2 1 0 0 0]
    ((comp ->element ->slot ->location) n m)))

;; FIXME: What happen when k > 2|x| - 2?
(defn ->add [k_ x-code y-code]
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
      (->add j
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
