(ns nullstellensatz.labeled-graph.connected
  (:require [clojure.math.combinatorics :refer [count-combinations]]))

(defrecord Pack [n-value
                 k-value
                 p-k-value
                 subset-value
                 binomial-value])

(defrecord Structure [])

(def ^:private state (atom {:numeric {}
                            :constructive {}}))

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

(defn- ->subset-value [k] (->> k (Math/pow 2) dec bigint))

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

(defn- ->basic-store [n]
  (let [->values #(if (= 1 %) '(1) (range 1 %))]
    (->> n ->values vec (->packs n) (->>state :basic n))))

(defn- ->quantity [{:keys [n-value k-value p-k-value subset-value binomial-value]}]
  (if (< n-value 3)
    (->>state :numeric n-value 1)
    (let [k-quantity (get-in @state [:numeric k-value])
          p-k-quantity (get-in @state [:numeric p-k-value])]
      (*' k-quantity p-k-quantity subset-value binomial-value))))

(defn- ->numeric-store [packs]
  (->> packs
       (map ->quantity)
       (apply +')
       (->>state :numeric (get-in packs [0 :n-value]))))

(defn ->size [n]
  (let [->answer (comp ->numeric-store ->basic-store)]
    (->> n inc (range 1) (mapv ->answer) last)))

(comment (let [p 90] (->size p)))
(comment (clojure.pprint/pprint (:numeric @state)))
