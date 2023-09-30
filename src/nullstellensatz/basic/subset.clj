(ns nullstellensatz.basic.subset)

(defn enumerate [n] (->> n (Math/pow 2) Math/round))

(defn generate [n m]
  (loop [n n m m answer []]
    (if (zero? n) answer
        (let [size (-> n dec enumerate)
              jump? (< size m)]
          (recur (dec n)
                 (if jump? (- m size) m)
                 (if jump? (cons n answer) answer))))))
