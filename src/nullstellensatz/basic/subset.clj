(ns nullstellensatz.basic.subset)

(def ^:private state (atom {:code {}
                            :stock {}}))

(defn ->>state [k i v]
  (swap! state assoc-in [k i] v) v)

(defn state->> [k] (get @state k))

(defn enumerate [n] (->> n (Math/pow 2) Math/round))

(defn search [n m])

(defn generate [n m])

(comment (search 10 100))
(comment (state->> :stock))
