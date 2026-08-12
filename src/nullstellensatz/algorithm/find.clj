(ns nullstellensatz.algorithm.find)

(defn maximum [items]
  (when (seq items)
    (reduce max items)))