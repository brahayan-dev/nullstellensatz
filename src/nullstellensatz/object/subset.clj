(ns nullstellensatz.object.subset
  (:require [schema.core :as s]
            [nullstellensatz.common :as common]))

(defn enumerate [n] (bit-shift-left 1 n))

(defn generate [n m]
  (loop [n n m m answer []]
    (if (zero? n) answer
        (let [n_ (dec n)
              size (bit-shift-left 1 n_)
              jump? (< size m)]
          (recur n_
                 (if jump? (-' m size) m)
                 (if jump? (cons n answer) answer))))))

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
