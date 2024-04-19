(ns nullstellensatz.object.subset
  (:require [schema.core :as s]
            [nullstellensatz.common :refer [->options]]))

(s/defschema EnumerateSchema
  {:n s/Int})

(s/defschema GenerateSchema
  {:n s/Int :m s/Int})

(def generate-options (->options :g "a" "subsets" GenerateSchema))
(def enumerate-options (->options :e "a" "subsets" EnumerateSchema))

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
