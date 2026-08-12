(ns nullstellensatz.object.complete-linked-diagram
  (:require [nullstellensatz.common :as common]
            [schema.core :as s]))

(defn- enumerate*
  "Number of complete linked diagrams on n arcs: product of (2k-1) for k=1..n."
  [n]
  (reduce *' (map #(dec (*' 2 %)) (range 1 (inc n)))))

(def enumerate (memoize enumerate*))

(defn- ->updated-structure [[free open] structure]
  (letfn [(->new-structure [pair]
            (if (some #{open} pair)
              (->> pair
                   (filter #(not= % open))
                   (cons free)) pair))]
    (map (comp set ->new-structure) structure)))

(defn- ->products [m]
  (loop [k 1 answer []]
    (if (= k m) answer
        (let [k-val (enumerate k)]
          (recur (inc k) (cons k-val answer))))))

(defn- ->code [p products representation]
  (if (empty? products)
    (cons 0 representation)
    (let [k (first products)
          x (rem p k)
          y (quot p k)]
      (recur x (rest products) (cons y representation)))))

(defn unrank [n m]
  ((comp vec ->code) m (->products n) []))

(defn- ->structure [code pairs]
  (loop [code code pairs pairs answer []]
    (if (empty? code) answer
        (let [open-item (first code)
              [free-item close-item] (first pairs)
              new-pair #{open-item close-item}
              structure (->> answer
                             (->updated-structure [free-item open-item])
                             (cons new-pair))]
          (recur (rest code) (rest pairs) structure)))))

(defn generate [n m]
  (let [n_ (*' 2 n)
        a (range 0 n_ 2)
        b (range 1 n_ 2)
        code (unrank n m)
        pairs (mapv vector a b)]
    ((comp set ->structure) code pairs)))

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
