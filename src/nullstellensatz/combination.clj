(ns nullstellensatz.combination)

(defn- factorial [n]
  (->> n inc (range 2) (reduce *')))

(defn enumerate [n k]
  (let [k! (factorial k)
        n! (factorial n)
        p! (factorial (-' n k))]
    (quot n! (*' k! p!))))

(defn generate [n k m]
  (loop [n n k k m m answer []]
    (if (or (zero? n) (zero? k)) answer
        (let [n_ (dec n)
              size (enumerate n_ k)
              same? (= n k)
              jump? (< size m)]
          (recur n_
                 (if jump? (dec k) k)
                 (if jump? (-' m size) m)
                 (if (or same? jump?) (cons n answer) answer))))))
