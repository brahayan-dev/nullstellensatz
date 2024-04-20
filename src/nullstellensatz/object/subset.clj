(ns nullstellensatz.object.subset
  (:require [schema.core :as s]
            [nullstellensatz.common :refer [->options ->identifier]]))

(s/defschema EnumerateSchema
  {:n s/Int})

(s/defschema GenerateSchema
  {:n s/Int :m s/Int})

(def generate-id (->identifier ::generate))
(def enumerate-id (->identifier ::enumerate))
(def generate-options (->options ::generate GenerateSchema))
(def enumerate-options (->options ::enumerate EnumerateSchema))

(defn enumerate [n] (->> n (Math/pow 2) Math/round))

(defn generate [n m]
  (loop [n n m m answer []]
    (if (zero? n) answer
        (let [n_ (dec n)
              size (enumerate n_)
              jump? (< size m)]
          (recur n_
                 (if jump? (-' m size) m)
                 (if jump? (cons n answer) answer))))))

(defn ->enumerated [{:keys [n]}] (enumerate n))
(defn ->generated [{:keys [n m]}] (generate n m))
