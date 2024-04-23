(ns nullstellensatz.algorithm.find)

(defn- helper [items n answer]
  (if (zero? n) answer
      (let [[head & tail] items]
        (recur tail
               (count tail)
               (if (> head answer) head answer)))))

(defn ->maximum [items]
  (cond
    (nil? items) nil
    (empty? items) nil
    :else (let [[head & tail] items
                n (count tail)]
            (helper tail n head))))
