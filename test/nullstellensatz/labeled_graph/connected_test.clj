(ns nullstellensatz.labeled-graph.connected-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.labeled-graph.connected :as connected]))

(deftest check->stored-packs
  (is (match? [{:n-value 1 :k-value 1 :p-k-value 0 :subset-value 1 :binomial-value 1}]
              (connected/->stored-packs 1)))
  (is (match? [{:n-value 2 :k-value 1 :p-k-value 1 :subset-value 1 :binomial-value 1}]
              (connected/->stored-packs 2)))
  (is (match? [{:n-value 3 :k-value 1 :p-k-value 2 :subset-value 1 :binomial-value 1}
               {:n-value 3 :k-value 2 :p-k-value 1 :subset-value 3 :binomial-value 1}]
              (connected/->stored-packs 3)))
  (is (match? [{:n-value 4 :k-value 1 :p-k-value 3 :subset-value 1 :binomial-value 1}
               {:n-value 4 :k-value 2 :p-k-value 2 :subset-value 3 :binomial-value 2}
               {:n-value 4 :k-value 3 :p-k-value 1 :subset-value 7 :binomial-value 1}]
              (connected/->stored-packs 4)))
  (is (match? [{:n-value 5 :k-value 1 :p-k-value 4 :subset-value 1 :binomial-value 1}
               {:n-value 5 :k-value 2 :p-k-value 3 :subset-value 3 :binomial-value 3}
               {:n-value 5 :k-value 3 :p-k-value 2 :subset-value 7 :binomial-value 3}
               {:n-value 5 :k-value 4 :p-k-value 1 :subset-value 15 :binomial-value 1}]
              (connected/->stored-packs 5))))

(deftest check->stored-stocks
  (let [->terms (fn [answer v] (connected/->stored-stocks v) answer)]
    (connected/clear-state)
    (is (match? (connected/state->> :terms)
                (->terms {1 [1]}
                         [{:n-value 1 :k-value 1 :p-k-value 0 :subset-value 1 :binomial-value 1}])))
    (is (match? (connected/state->> :terms)
                (->terms {1 [1]}
                         [{:n-value 2 :k-value 1 :p-k-value 1 :subset-value 1 :binomial-value 1}])))
    (is (match? (connected/state->> :terms)
                (->terms {1 [1] 2 [1] 3 [1 3]}
                         [{:n-value 3 :k-value 1 :p-k-value 2 :subset-value 1 :binomial-value 1}
                          {:n-value 3 :k-value 2 :p-k-value 1 :subset-value 3 :binomial-value 1}])))
    (is (match? (connected/state->> :terms)
                (->terms {1 [1] 2 [1] 3 [1 3] 4 [4 6 28]}
                         [{:n-value 4 :k-value 1 :p-k-value 3 :subset-value 1 :binomial-value 1}
                          {:n-value 4 :k-value 2 :p-k-value 2 :subset-value 3 :binomial-value 2}
                          {:n-value 4 :k-value 3 :p-k-value 1 :subset-value 7 :binomial-value 1}])))
    (is (match? (->terms {1 [1] 2 [1] 3 [1 3] 4 [4 6 28] 5 [38 36 84 570]}
                         [{:n-value 5 :k-value 1 :p-k-value 4 :subset-value 1 :binomial-value 1}
                          {:n-value 5 :k-value 2 :p-k-value 3 :subset-value 3 :binomial-value 3}
                          {:n-value 5 :k-value 3 :p-k-value 2 :subset-value 7 :binomial-value 3}
                          {:n-value 5 :k-value 4 :p-k-value 1 :subset-value 15 :binomial-value 1}])
                (connected/state->> :terms)))))

(deftest check->size
  (is (= 1 (connected/->size 1)))
  (is (= 1 (connected/->size 2)))
  (is (= 4 (connected/->size 3)))
  (is (= 38 (connected/->size 4)))
  (is (= 728 (connected/->size 5)))
  (is (= 26704 (connected/->size 6))))

(deftest check->code
  (is (= {:n-value 3
          :k-value 1
          :labels #{}
          :nodes #{}
          :left-graph {}
          :right-graph {}} (connected/->code 3 1)))
  (is (= {:n-value 3
          :k-value 2
          :labels #{}
          :nodes #{}
          :left-graph {}
          :right-graph {}} (connected/->code 3 2))))
