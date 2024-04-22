(ns nullstellensatz.algorithm.find)

(defn ->maximum [items]
  (loop [items items n (count items) answer nil]
    (if (zero? n) answer
        (let [[head & tail] items
              n_ (count tail)
              answer_ (cond
                        (nil? answer) head
                        (> head answer) head
                        :else answer)]
          (recur tail n_ answer_)))))
