(ns nullstellensatz.object.subset)

(defn enumerate [n] (->> n (Math/pow 2) Math/round))

(defn generate [n m]
  (loop [n n m m answer []]
    (if (zero? n) answer
        (let [n_ (dec n)
              size (enumerate n_)
              jump? (< size m)]
          (recur n_
                 (if jump? (- m size) m)
                 (if jump? (cons n answer) answer))))))
