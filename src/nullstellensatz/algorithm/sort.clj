(ns nullstellensatz.algorithm.sort)

(defn standard [items]
  (sort items))

(defn quick [items]
  (if (<= (count items) 1)
    items
    (let [pivot (first items)
          tail (rest items)
          smaller (filter #(< % pivot) tail)
          larger (filter #(>= % pivot) tail)]
      (concat (quick smaller) [pivot] (quick larger)))))

