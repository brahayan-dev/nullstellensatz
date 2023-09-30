(ns nullstellensatz.basic.combination)

(defn- factorial [n]
  (->> n inc (range 2) (reduce *')))

(defn enumerate [n k]
  (let [k! (factorial k)
        n! (factorial n)
        p! (factorial (-' n k))]
    (quot n! (*' k! p!))))

(defn generate [n k m])
