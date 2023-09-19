(ns nullstellensatz.labeled-graph.connected-test
  (:require [nullstellensatz.labeled-graph.connected :as connected]
            [clojure.test :refer [deftest is]]))

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
