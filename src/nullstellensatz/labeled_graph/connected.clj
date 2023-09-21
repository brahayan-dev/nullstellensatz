(ns nullstellensatz.labeled-graph.connected
  (:require [clojure.math.combinatorics :refer [count-combinations]]))

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

(defn- ->>state [k i v]
  (swap! state assoc-in [k i] v) v)

(defn state->> [k] (get @state k))

(defn clear-state []
  (reset! state empty-state))

(defn- ->subset-value [k] (->> k (Math/pow 2) Math/round dec))

(defn- ->binomial-value [p k]
  (let [->combinations #(->> k dec (count-combinations %))]
    (-> p (- 2) range ->combinations)))

(defn- ->packs [n k-values]
  (let [->pack (fn [k]
                 (Pack. n k
                        (- n k)
                        (->subset-value k)
                        (->binomial-value n k)))]
    (mapv ->pack k-values)))

(defn ->stored-packs [n]
  (let [->k-values #(if (= 1 %) '(1) (range 1 %))]
    (->> n ->k-values vec (->packs n) (->>state :polynomials n))))

(defn- ->quantity [{:keys [n-value k-value p-k-value subset-value binomial-value]}]
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

(defn- ->polynomials [n]
  (->> n inc (range 1) (map ->stored-packs)))

(defn ->size [n]
  (->> n ->polynomials (mapv ->stored-stocks) last))

(defn- ->k-value [polynomial terms n m]
  (let [items (rest polynomial)
        value (-> polynomial first ->quantity)]
    (if (<= m value)
      (->> terms (->>state :terms n) count inc)
      (recur items (conj terms value) n (- m value)))))

(defn ->code [n m]
  (let [items (->polynomials n)
        k (-> items (nth n) (->k-value [] n m))
        labels #{}]
    (Structure. n k labels #{} {} {})))

(comment (nth [:a :b :c] 0))
(comment (clojure.pprint/pprint (->>state :terms 1 2)))
(comment (clojure.pprint/pprint @state))
