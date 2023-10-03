(ns nullstellensatz.labeled-graph.connected
  (:require [nullstellensatz.basic.subset :as subset]
            [nullstellensatz.basic.combination :as combination]))

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

(defn ->>state [k i v]
  (swap! state assoc-in [k i] v) v)

(defn state->>
  ([k default] (get @state k default))
  ([k n default] (get-in @state [k n] default)))

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

(defn ->outcomes [n]
  (state->> :outcomes n
            (->> n
                 inc
                 ->items
                 (mapv ->packs)
                 (->>state :outcomes n))))

#_(defn ->quantity [n]
    (state->> :quantities n
              (if (< n 3)
                (state->> :quantities n 1)
                (let [items (->polynomial n)]
                  (*' k-quantity p-k-quantity subset-value binomial-value)))))

(defn clear-state []
  (reset! state empty-state))

(defn ->stored-stocks [packs]
  (let [index (get-in packs [0 :n-value])]
    (->> packs
         (map ->quantity)
         (->>state :terms index)
         (apply +')
         (->>state :quantities index))))

(defn ->size [n]
  (->> n (state->> :polynomials) (mapv ->stored-stocks) last))

(defn ->k-value [polynomial n m]
  (loop [n n m m p polynomial out []]
    (let [v (-> p first ->quantity)]
      (if (= 0 n)
        (->> out (->>state :outcomes n))
        (recur (dec n)
               (- m v)
               (rest p)
               (cons n out))))))

(defn ->code [n m]
  (let [items (->polynomials n)
        k (-> items (nth n) (->k-value n m))
        labels #{}]
    (Structure. n k labels #{} {} {})))

(comment (nth [:a :b :c] 0))
(comment (clojure.pprint/pprint (->>state :terms 1 2)))
(comment (clojure.pprint/pprint @state))
