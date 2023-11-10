(ns nullstellensatz.noncrossing-linked-diagram-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.noncrossing-linked-diagram :refer [encode enumerate]]))

(deftest check-enumerate
  (is (= 1 (enumerate 0)))
  (is (= 1 (enumerate 1)))
  (is (= 2 (enumerate 2)))
  (is (= 5 (enumerate 3)))
  (is (= 14 (enumerate 4)))
  (is (= 42 (enumerate 5)))
  (is (= 132 (enumerate 6))))

(deftest check-encode
  (is (match? [0 0 0 0] (encode 0 0)))
  (is (match? [1 0 0 0] (encode 1 0)))
  (is (match? [2 0 0 0] (encode 2 0))))
