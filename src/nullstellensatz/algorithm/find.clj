(ns nullstellensatz.algorithm.find)

(defn- max-loop [items answer]
  (if (empty? items) answer
      (let [[head & tail] items]
        (recur tail
               (if (> head answer) head answer)))))

(defn maximum [items]
  (cond
    (nil? items) nil
    (empty? items) nil
    :else (let [[head & tail] items]
             (max-loop tail head))))
