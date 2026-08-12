(ns nullstellensatz.object.combination
  (:require [schema.core :as s]
            [nullstellensatz.common :as common]))

(defn- enumerate*
  "Binomial coefficient C(n,k) via telescoping product.
  Avoids computing full factorials, keeping intermediate numbers smaller."
  [n k]
  (cond
    (neg? k) 0
    (or (neg? n) (> k n)) 0
    (or (zero? k) (= k n)) 1
    :else (loop [i 0 acc 1]
            (if (= i k) acc
                (recur (inc i) (quot (*' acc (-' n i)) (inc i)))))))

(def enumerate (memoize enumerate*))

(defn generate [n k m]
  (loop [n n k k m m answer []]
    (if (or (zero? n) (zero? k)) (vec answer)
        (let [n_ (dec n)
              size (enumerate n_ k)
              same? (= n k)
              jump? (< size m)]
          (recur n_
                 (if jump? (dec k) k)
                 (if jump? (-' m size) m)
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
