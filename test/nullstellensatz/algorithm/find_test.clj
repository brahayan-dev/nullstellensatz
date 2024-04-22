(ns nullstellensatz.algorithm.find-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.algorithm.find :refer [->maximum]]))

(deftest check->maximum
  (is (match? nil (->maximum [])))
  (is (match? nil (->maximum nil)))
  (is (match? 1 (->maximum [1])))
  (is (match? 3 (->maximum [1 2 3])))
  (is (match? 0 (->maximum [-10 0 0])))
  (is (match? 5 (->maximum [5 3 2 4 1])))
  (is (match? 6 (->maximum [6 3 3 1 2])))
  (is (match? 20 (->maximum [7 3 3 2 20]))))
