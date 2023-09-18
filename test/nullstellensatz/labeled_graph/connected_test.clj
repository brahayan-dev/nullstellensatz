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
  (is (= [0 [1 0] [2 0]] (connected/->code 3 1)))
  (is (= [0 [2 0] [1 0]] (connected/->code 3 2)))
  (is (= [1 [2 0] [1 0]] (connected/->code 3 3)))
  (is (= [2 [2 0] [1 0]] (connected/->code 3 4)))
  (is (= [0 [3 1] [2 0]] (connected/->code 4 20)))
  (is (= [3 [3 1] [3 2]] (connected/->code 5 383)))
  (is (= [3 [3 2] [4 20]] (connected/->code 5 720))))
