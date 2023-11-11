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
  (is (match? [2 0 0 0] (encode 2 0)))
  (is (match? [2 0 1 0] (encode 2 1)))
  (is (match? [3 0 0 0] (encode 3 0)))
  (is (match? [3 0 0 1] (encode 3 1)))
  (is (match? [3 0 1 0] (encode 3 2)))
  (is (match? [3 1 1 0] (encode 3 3)))
  (is (match? [3 2 1 0] (encode 3 4)))
  (is (match? [4 0 0 0] (encode 4 0)))
  (is (match? [4 0 0 1] (encode 4 1)))
  (is (match? [4 0 0 2] (encode 4 2)))
  (is (match? [4 0 0 3] (encode 4 3)))
  (is (match? [4 0 0 4] (encode 4 4)))
  (is (match? [4 0 1 0] (encode 4 5)))
  (is (match? [4 1 0 1] (encode 4 6)))
  (is (match? [4 1 1 0] (encode 4 7)))
  (is (match? [4 2 1 0] (encode 4 8)))
  (is (match? [4 2 2 0] (encode 4 9)))
  (is (match? [4 3 1 0] (encode 4 10)))
  (is (match? [4 3 2 0] (encode 4 11)))
  (is (match? [4 3 3 0] (encode 4 12)))
  (is (match? [4 3 4 0] (encode 4 13))))
