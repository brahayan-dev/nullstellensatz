(ns nullstellensatz.algorithm.maximum)

(defn find [items]
  (loop [items items n (count items) answer nil]
    (if (zero? n) answer
        (let [[head & tail] items
              updated-answer (cond
                               (nil? head) answer
                               (nil? answer) head
                               (> head answer) head
                               :else answer)]
          (recur tail n updated-answer)))))
