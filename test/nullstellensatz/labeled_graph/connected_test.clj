(ns nullstellensatz.labeled-graph.connected-test
  (:require [nullstellensatz.labeled-graph.connected :as connected]
            [clojure.test :refer [deftest is]]))

(deftest check->size
  (is (= 1 (connected/->size 1)))
  (is (= 1 (connected/->size 2)))
  (is (= 4 (connected/->size 3)))
  (is (= 38 (connected/->size 4)))
  (is (= 728 (connected/->size 5)))
  #_(is (= 2830 (connected/->size 6))))
