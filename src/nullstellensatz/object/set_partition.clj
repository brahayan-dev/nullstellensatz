(ns nullstellensatz.object.set-partition
  (:require [schema.core :as s]
            [nullstellensatz.common :as common]))

(defn- ->restricted-growth-row [n i j answer]
  (if (> j (-' n i)) answer
      (let [value (+' (get-in answer [(dec i) (inc j)])
                      (*' j (get-in answer [(dec i) j])))]
        (recur n i
               (inc j)
               (-> answer (assoc-in [i j] value))))))

(defn- ->restricted-growth-table [n]
  (loop [i 1 answer [(-> n inc (repeat 1) vec)]]
    (if (> i n) answer
        (recur (inc i)
               (->restricted-growth-row n i 0 (conj answer []))))))

(defn enumerate [n]
  (let [table (->restricted-growth-table n)]
    (get-in table [n 0])))

(defn unrank [n m]
  (let [table (->restricted-growth-table n)]
    (loop [i 2 j 1 m m answer {1 1}]
      (if (> i n) answer
          (let [d (get-in table [(-' n i) j])
                jump? (>= m (*' j d))
                ->add #(assoc answer i %)]
            (recur (inc i)
                   (if jump? (inc j) j)
                   (if jump? (-' m (*' j d)) (mod m d))
                   (if jump?
                     (->add (inc j))
                     (->add (inc (quot m d))))))))))

(defn generate [n m]
  (let [code (unrank n m)
        structure (as-> code $
                    (vals $)
                    (apply max $)
                    (repeat $ []) (vec $))]
    (reduce (fn [s [k v]] (update s (dec v) conj k)) structure code)))

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
