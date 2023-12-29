(ns nullstellensatz.object.complete-linked-diagram-test
  (:require [clojure.test :refer [deftest is]]
            [nullstellensatz.object.complete-linked-diagram :as complete]))

(deftest check-unrank
  (is (= [0 0 0 0] (complete/unrank 8 0)))
  (is (= [0 2 4 1] (complete/unrank 8 29)))
  (is (= [0 1 2 2] (complete/unrank 8 37)))
  (is (= [0 2 4 6] (complete/unrank 8 104))))

(deftest check-enumerate
  (is (= 15 (complete/enumerate 3)))
  (is (= 105 (complete/enumerate 4)))
  (is (= 945 (complete/enumerate 5)))
  (is (= 10395 (complete/enumerate 6)))
  (is (= 6190283353629375 (complete/enumerate 15))))

(deftest check-generate
  (is (= #{#{0 7} #{1 2} #{3 4} #{5 6}} (complete/generate 8 0)))
  (is (= #{#{0 6} #{1 7} #{2 3} #{4 5}} (complete/generate 8 29)))
  (is (= #{#{0 4} #{1 3} #{2 7} #{5 6}} (complete/generate 8 37)))
  (is (= #{#{0 1} #{2 3} #{4 5} #{6 7}} (complete/generate 8 104))))
