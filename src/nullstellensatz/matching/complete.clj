(ns nullstellensatz.matching.complete)

(defn- generate-odd-numbers [n]
  (range 1 n 2))

(defn- multiply-values [odd-numbers]
  (letfn [(->values [[k :as acc] n]
            (-> k nil? (if 1 k) (*' n) (cons acc)))]
    (reduce ->values [] odd-numbers)))

(defn- generate-code [p products representation]
  (cond
    (empty? products) (cons 0 representation)
    :else (let [k (first products)
                x (rem p k)
                y (quot p k)]
            (recur x (rest products) (cons y representation)))))

(defn- update-structure [[free open] structure]
  (letfn [(->new-structure [pair]
            (if (some #{open} pair)
              (->> pair
                   (filter #(not= % open))
                   (cons free)) pair))]
    (map (comp set ->new-structure) structure)))

(defn- generate-structure [representation pairs structure]
  (cond
    (empty? representation) structure
    :else (let [open-item (first representation)
                [free-item close-item] (first pairs)
                new-pair (set [open-item close-item])
                new-structure (->> structure
                                   (update-structure [free-item open-item])
                                   (cons new-pair))]
            (recur (rest representation) (rest pairs) new-structure))))

(defn ->size [n]
  (reduce *' 1 (generate-odd-numbers n)))

(defn ->code [nucleotids position]
  (let [products (-> nucleotids
                     generate-odd-numbers multiply-values rest)]
    (generate-code position products [])))

(defn ->structure [nucleotids position]
  (let [a (range 0 nucleotids 2)
        b (range 1 nucleotids 2)
        pairs (map vector a b)
        representation (->code nucleotids position)]
    ((comp set generate-structure) representation pairs [])))

