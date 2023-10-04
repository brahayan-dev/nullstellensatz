(ns nullstellensatz.labeled-graph.connected
  (:require
   [nullstellensatz.basic.combination :as combination]
   [nullstellensatz.basic.subset :as subset]))

(defrecord Pack [n-value
                 k-value
                 n-k-value
                 subset-value
                 binomial-value])

(defrecord Structure [n-value
                      k-value
                      labels
                      nodes
                      left-graph
                      right-graph])

(def ^:private empty-state {:packs {}
                            :outcomes {}})

(def ^:private state (atom empty-state))

(defn- debug-log
  ([x]
   (debug-log "X" x))
  ([title x]
   (let [t (str "|---|" title "|-------------|")]
     (clojure.pprint/pprint t)
     (clojure.pprint/pprint x)
     (clojure.pprint/pprint t) x)))

(defn ->>state [field index v]
  (swap! state assoc-in [field index] v) v)

(defn state->>
  ([field]
   (get @state field))
  ([field index]
   (get-in @state [field index]))
  ([field index default]
   (get-in @state [field index]
           (->>state field index default))))

(defn ->subset [k]
  (->> k subset/enumerate dec))

(defn ->binomial [p k]
  (combination/enumerate (- p 2) (dec k)))

(defn ->items [n]
  (if (= 1 n) '(1) (range 1 n)))

(defn ->packs [n]
  (letfn [(->answer [k]
            (state->> :packs [n k]
                      (Pack. n k
                             (- n k)
                             (->subset k)
                             (->binomial n k))))]
    (->> n ->items (mapv ->answer))))

(defn ->size [n]
  (->> :outcomes
       state->>
       (into [])
       (filter (fn [[[n-value _] _]] (= n-value n)))
       (map (fn [[_ v]] v))
       (reduce +)))

(defn ->outcomes [{:keys [n-value k-value n-k-value subset-value binomial-value]}]
  (state->> :outcomes [n-value k-value]
            (if (< n-value 3) 1
                (let [k-outcome (->size k-value)
                      n-k-outcome (->size n-k-value)]
                  (*' k-outcome n-k-outcome subset-value binomial-value)))))

(defn ->quantities [n]
  (->> n
       inc
       ->items
       (mapv ->packs)
       flatten
       (mapv ->outcomes)))

(defn enumerate [n] (->quantities n) (->size n))

#_(defn ->k-value [polynomial n m]
    (loop [n n m m p polynomial out []]
      (let [v (-> p first ->quantity)]
        (if (= 0 n)
          (->> out (->>state :outcomes n))
          (recur (dec n)
                 (- m v)
                 (rest p)
                 (cons n out))))))

#_(defn generate [n m]
    (let [items (->polynomials n)
          k (-> items (nth n) (->k-value n m))
          labels #{}]
      (Structure. n k labels #{} {} {})))

(comment (clojure.pprint/pprint (->quantities 5)))
(comment (clojure.pprint/pprint @state))
