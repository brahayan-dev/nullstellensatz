(ns nullstellensatz.labeled-graph.connected
  (:require
   [nullstellensatz.basic.combination :as combination]
   [nullstellensatz.basic.subset :as subset]))

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

(defn- find-term [n m]
  (loop [n n m m k 1]
    (let [outcome (state->> :outcomes [n k])
          jump? (< m outcome)
          m_ (- m outcome)
          k_ (inc k)]
      (if (not jump?) [k_ m_]
          (recur n (if jump? m_ m) k_)))))

(defn find-labels [n k m]
  (let [outcome (state->> :outcomes [n k])
        m_ (-> m (quot outcome) inc)]
    [(combination/generate (- n 2) (dec k) m_) (rem m outcome)]))

(defn find-nodes [n k m]
  (let [{:keys [subset-value n-k-value]} (state->> :polynomials [n k])
        g-1 (enumerate k)
        g-2 (enumerate n-k-value)
        m_ (->> (*' subset-value g-1 g-2) (quot m) inc)]
    [(subset/generate (dec n) m_) (rest m m_)]))

(defn generate [n m]
  (let [_ (->quantities n)
        [k m-1] (find-term n m)
        [labels m-2] (find-labels n k m-1)
        [nodes _m-3] (find-nodes n k m-2)]
    (Structure. n k labels nodes)))

(comment (clojure.pprint/pprint (generate 4 2)))
(comment (clojure.pprint/pprint @state))
