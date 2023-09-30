(ns nullstellensatz.basic.subset)

(defn enumerate [n] (->> n (Math/pow 2) Math/round))

(defn search [n m]
  (loop [n n m m answer []]
    (if (zero? n) answer
        (let [size (-> n dec enumerate)
              jump? (< size m)
              value (if jump? 1 0)]
          (recur (dec n) (if jump? (- m size) m) (conj answer value))))))

(defn generate [n m]
  (let [code (search n m)
        ->selected (fn [index v] (when (= v 1) (inc index)))
        ->xform (comp (map-indexed ->selected) (remove nil?))]
    (->> code (eduction ->xform) vec)))
