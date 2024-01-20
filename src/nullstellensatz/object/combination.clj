(ns nullstellensatz.object.combination)

(defn- factorial ^Long [^Long n]
  (loop [i 2 acc 1]
    (if (or (<= n 1) (> i n)) acc
        (recur (inc i) (*' acc i)))))

(defn- rising-factorial ^Long [^Long n ^Long k]
  (loop [i 0 acc 1]
    (if (> i (dec k)) acc
        (recur (inc i) (*' acc (-' n i))))))

(defn enumerate ^Long [^Long n ^Long k]
  (if (neg? k) 0
      (let [k-val (factorial k)
            p-val (rising-factorial n k)]
        (quot p-val k-val))))

(defn generate ^Long [^Long n ^Long k ^Long i]
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
