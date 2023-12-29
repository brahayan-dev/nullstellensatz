(ns nullstellensatz.object.combination)

(defn- factorial [n]
  (->> n inc (range 2) (reduce *')))

(defn enumerate [n k]
  (let [k-val (factorial k)
        n-val (factorial n)
        p-val (factorial (-' n k))]
    (quot n-val (*' k-val p-val))))

(defn generate [n k i]
  (loop [n n k k index i answer []]
    (if (or (zero? n) (zero? k)) (vec answer)
        (let [n_ (dec n)
              size (enumerate n_ k)
              same? (= n k)
              jump? (< size index)]
          (recur n_
                 (if jump? (dec k) k)
                 (if jump? (-' index size) index)
                 (if (or same? jump?) (cons n answer) answer))))))
