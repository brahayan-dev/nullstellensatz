(ns nullstellensatz.complete-linked-diagram-test
  (:require [clojure.test :refer [deftest is]]
            [nullstellensatz.complete-linked-diagram :as complete]))

(deftest check->code
  (is (= [0 0 0 0] (complete/->code 8 0)))
  (is (= [0 2 4 1] (complete/->code 8 29)))
  (is (= [0 1 2 2] (complete/->code 8 37)))
  (is (= [0 2 4 6] (complete/->code 8 104))))

(deftest check-enumerate
  (is (= 15 (complete/enumerate 3)))
  (is (= 105 (complete/enumerate 4)))
  (is (= 945 (complete/enumerate 5)))
  (is (= 10395 (complete/enumerate 6)))
  (is (= 6190283353629375 (complete/enumerate 15))))

(deftest check->structure
  (is (= #{#{0 7} #{1 2} #{3 4} #{5 6}} (complete/->structure 8 0)))
  (is (= #{#{0 6} #{1 7} #{2 3} #{4 5}} (complete/->structure 8 29)))
  (is (= #{#{0 4} #{1 3} #{2 7} #{5 6}} (complete/->structure 8 37)))
  (is (= #{#{0 1} #{2 3} #{4 5} #{6 7}} (complete/->structure 8 104))))
