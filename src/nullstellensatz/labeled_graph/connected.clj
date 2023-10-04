(ns nullstellensatz.labeled-graph.connected
  (:require
   [nullstellensatz.basic.combination :as combination]
   [nullstellensatz.basic.subset :as subset]))

(defrecord Pack [n-value
                 k-value
                 p-k-value
                 subset-value
                 binomial-value])

(defrecord Structure [n-value
                      k-value
                      labels
                      nodes
                      left-graph
                      right-graph])

(def ^:private empty-state {:terms {}
                            :outcomes {}
                            :quantities {}
                            :polynomials {}})

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

(defn state->> [k] (get @state k))

(defn clear-state []
  (reset! state empty-state))

(defn ->subset-value [k] (->> k subset/enumerate dec))

(defn ->binomial-value [p k]
  (let [p_ (- p 2)
        k_ (dec k)]
    (combination/enumerate p_ k_)))

(defn ->packs [n k-values]
  (let [->pack (fn [k]
                 (Pack. n k
                        (- n k)
                        (->subset-value k)
                        (->binomial-value n k)))]
    (mapv ->pack k-values)))

(defn ->stored-packs [n]
  (let [->k-values #(if (= 1 %) '(1) (range 1 %))]
    (->> n ->k-values vec (->packs n) (->>state :polynomials n))))

(defn ->quantity [{:keys [n-value k-value p-k-value subset-value binomial-value]}]
  (if (< n-value 3)
    (->>state :quantities n-value 1)
    (let [k-quantity (get-in @state [:quantities k-value])
          p-k-quantity (get-in @state [:quantities p-k-value])]
      (*' k-quantity p-k-quantity subset-value binomial-value))))

(defn ->stored-stocks [packs]
  (let [index (get-in packs [0 :n-value])]
    (->> packs
         (map ->quantity)
         (->>state :terms index)
         (apply +')
         (->>state :quantities index))))

(defn ->polynomials [n]
  (->> n inc (range 1) (map ->stored-packs)))

(defn enumerate [n]
  (->> n ->polynomials (mapv ->stored-stocks) last))

(defn ->k-value [polynomial n m]
  (loop [p polynomial answer [] n n m m]
    (let [items (rest p)
          size 3 #_(-> p first ->quantity)]
      (if (zero? n)
        (->> answer (->>state :outcomes n) count inc)
        (recur items (conj answer size) (dec n) (- m size))))))

(defn generate [n m]
  (let [items (->polynomials n)
        k (-> items (nth n) (->k-value n m))
        labels #{}]
    (Structure. n k labels #{} {} {})))

(comment (nth [:a :b :c] 0))
(comment (clojure.pprint/pprint (->>state :terms 1 2)))
(comment (clojure.pprint/pprint @state))
