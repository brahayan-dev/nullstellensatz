(ns nullstellensatz.object.complete-linked-diagram-test
  (:require [clojure.test :refer [deftest is]]
            [matcher-combinators.test :refer [match?]]
            [nullstellensatz.object.complete-linked-diagram :as complete]))

(deftest check-enumerate
  (is (= 15 (complete/enumerate 3)))
  (is (= 105 (complete/enumerate 4)))
  (is (= 945 (complete/enumerate 5)))
  (is (= 10395 (complete/enumerate 6)))
  (is (= 6190283353629375 (complete/enumerate 15))))

(deftest check-unrank
  (is (match? [0 0 0 0] (complete/unrank 4 0)))
  (is (match? [0 2 4 1] (complete/unrank 4 29)))
  (is (match? [0 1 2 2] (complete/unrank 4 37)))
  (is (match? [0 2 4 6] (complete/unrank 4 104))))

(deftest check-generate
  (is (match? #{#{0 7} #{1 2} #{3 4} #{5 6}} (complete/generate 4 0)))
  (is (match? #{#{0 6} #{1 7} #{2 3} #{4 5}} (complete/generate 4 29)))
  (is (match? #{#{0 4} #{1 3} #{2 7} #{5 6}} (complete/generate 4 37)))
  (is (match? #{#{0 1} #{2 3} #{4 5} #{6 7}} (complete/generate 4 104))))
