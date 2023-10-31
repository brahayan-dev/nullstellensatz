(ns nullstellensatz.labeled-connected-graph
  (:require
   [nullstellensatz.combination :as combination]
   [nullstellensatz.subset :as subset]))

(defrecord Pack [n-value
                 k-value
                 n-k-value
                 subset-value
                 binomial-value])

(defrecord Structure [n-value k-value labels nodes])

(def ^:private empty-state {:packs {}
                            :sizes {}
                            :outcomes {}})

(def ^:private state (atom empty-state))

(defn- ->>state [field index v]
  (swap! state assoc-in [field index] v) v)

(defn- state->>
  ([field]
   (get @state field))
  ([field index]
   (get-in @state [field index]))
  ([field index default]
   (get-in @state [field index]
           (->>state field index default))))

(defn ->items [n]
  (if (= 1 n) '(1) (range 1 n)))

(defn ->packs [n]
  (letfn [(->subset [k]
            (->> k subset/enumerate dec))
          (->binomial [p k]
            (combination/enumerate (- p 2) (dec k)))
          (->answer [k]
            (state->> :packs [n k]
                      (Pack. n k
                             (- n k)
                             (->subset k)
                             (->binomial n k))))]
    (->> n ->items (mapv ->answer))))

(defn- ->sizes [n]
  (state->> :sizes n
            (->> :outcomes
                 state->>
                 (into [])
                 (filter (fn [[[n-value _] _]] (= n-value n)))
                 (map (fn [[_ v]] v))
                 (reduce +))))

(defn- ->outcomes [{:keys [n-value k-value n-k-value subset-value binomial-value]}]
  (state->> :outcomes [n-value k-value]
            (if (< n-value 3) 1
                (let [k-outcome (->sizes k-value)
                      n-k-outcome (->sizes n-k-value)]
                  (*' k-outcome n-k-outcome subset-value binomial-value)))))

(defn ->quantities [n]
  (->> n
       inc
       ->items
       (mapv ->packs)
       flatten
       (mapv ->outcomes)))

(defn enumerate [n] (->quantities n) (->sizes n))

(defn ->term [n m]
  (loop [n n m m k 1]
    (let [outcome (state->> :outcomes [n k])
          jump? (>= m outcome)]
      (if jump? [k m]
          (recur n (if jump? (- m outcome) m) (inc k))))))

(defn generate [n m]
  (let [_ (->quantities n)
        [k m-rest] (->term n m)]
    (Structure. n k m-rest [])))

#_(defn generate [n m]
    (loop [k 1 m m labels [] nodes []]
      (if (> k n) (Structure. n k labels nodes)
          (let [size (enumerate k)
                jump? (< size m)]
            (recur (inc k)
                   (if jump? (- m size) m) labels nodes)))))

(comment (clojure.pprint/pprint (->quantities 5)))
(comment (clojure.pprint/pprint @state))
