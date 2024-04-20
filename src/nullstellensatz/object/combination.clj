(ns nullstellensatz.object.combination
  (:require [schema.core :as s]
            [nullstellensatz.common :as common]))

(defn- factorial [n]
  (loop [i 2 acc 1]
    (if (or (<= n 1) (> i n)) acc
        (recur (inc i) (*' acc i)))))

(defn- rising-factorial [n k]
  (loop [i 0 acc 1]
    (if (> i (dec k)) acc
        (recur (inc i) (*' acc (-' n i))))))

(defn enumerate [n k]
  (if (neg? k) 0
      (let [k-val (factorial k)
            p-val (rising-factorial n k)]
        (quot p-val k-val))))

(defn generate [n  k  m]
  (loop [n n k k index m answer []]
    (if (or (zero? n) (zero? k)) (vec answer)
        (let [n_ (dec n)
              size (enumerate n_ k)
              same? (= n k)
              jump? (< size index)]
          (recur n_
                 (if jump? (dec k) k)
                 (if jump? (-' index size) index)
                 (if (or same? jump?) (cons n answer) answer))))))

(s/defschema EnumerateSchema
  {:n s/Int :k s/Int})

(def export-enumerate
  (common/->Export
   EnumerateSchema ::enumerate
   (fn [{:keys [n k]}] (enumerate n k))))

(s/defschema GenerateSchema
  {:n s/Int :k s/Int :m s/Int})

(def export-generate
  (common/->Export
   GenerateSchema ::generate
   (fn [{:keys [n k m]}] (generate n k m))))
