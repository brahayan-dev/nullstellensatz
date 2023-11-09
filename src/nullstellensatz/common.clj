(ns nullstellensatz.common)

(defn ->ints-from-zero-to-val-less-one [n]
  (range 0 n))

(defn ->ints-from-one-to-val [n]
  (->> n inc (range 1)))

(defn ->ints-from-zero-to-val [n]
  (->> n inc (range 0)))

