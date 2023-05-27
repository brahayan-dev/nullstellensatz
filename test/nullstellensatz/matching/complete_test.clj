(ns nullstellensatz.matching.complete-test
  (:require [clojure.test :refer [deftest is]]
            [nullstellensatz.matching.complete :as complete]))

(deftest check->code
  (is (= [0 0 0 0] (complete/->code 8 0)))
  (is (= [0 2 4 1] (complete/->code 8 29)))
  (is (= [0 1 2 2] (complete/->code 8 37)))
  (is (= [0 2 4 6] (complete/->code 8 104))))

(deftest check->size
  (is (= 15 (complete/->size 6)))
  (is (= 105 (complete/->size 8)))
  (is (= 945 (complete/->size 10)))
  (is (= 10395 (complete/->size 12))))

(deftest check->structure
  (is (= #{#{0, 7}, #{1, 2}, #{3, 4}, #{5, 6}} (complete/->structure 8 0)))
  (is (= #{#{0, 6}, #{1, 7}, #{2, 3}, #{4, 5}} (complete/->structure 8 29)))
  (is (= #{#{0, 4}, #{1, 3}, #{2, 7}, #{5, 6}} (complete/->structure 8 37)))
  (is (= #{#{0, 1}, #{2, 3}, #{4, 5}, #{6, 7}} (complete/->structure 8 104))))

