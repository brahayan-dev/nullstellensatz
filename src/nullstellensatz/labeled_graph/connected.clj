(ns nullstellensatz.labeled-graph.connected)

(defrecord Pack [k-value
                 p-k-value
                 bin-value
                 factor-value])

(defn- ->factor-value [k] k)
(defn- ->binomial-value [k _] k)

(defn- ->packs [k-values p]
  (let [->pack (fn [k] (Pack. k
                             (- p k)
                             (->factor-value k)
                             (->binomial-value k p)))]
  (map ->pack k-values)))

(defn- generate-packs [p]
  (->> p (range 1) (->packs p)))

(defn ->size [p]
  (->> p generate-packs (map :k-value) (apply +')))
