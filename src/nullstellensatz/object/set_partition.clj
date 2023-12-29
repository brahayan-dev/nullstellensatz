(ns nullstellensatz.object.set-partition)

(defn- ->restricted-growth-row [n i j answer]
  (if (> j (- n i)) answer
      (let [value (+ (get-in answer [(dec i) (inc j)])
                     (* j (get-in answer [(dec i) j])))]
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

(defn unrank [n r]
  (let [table (->restricted-growth-table n)]
    (loop [i 2 j 1 r r answer {1 1}]
      (if (> i n) answer
          (let [d (get-in table [(- n i) j])
                jump? (>= r (* j d))
                ->add #(assoc answer i %)]
            (recur (inc i)
                   (if jump? (inc j) j)
                   (if jump? (- r (* j d)) (mod r d))
                   (if jump?
                     (->add (inc j))
                     (->add (inc (quot r d))))))))))

(defn generate [n r]
  (let [code (unrank n r)
        structure (as-> code $
                    (vals $)
                    (apply max $)
                    (repeat $ []) (vec $))]
    (reduce (fn [s [k v]] (update s (dec v) conj k)) structure code)))
