(ns nullstellensatz.noncrossing-linked-diagram-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.noncrossing-linked-diagram :refer
    [count-mountain-ranges encode enumerate unrank]]))

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

(deftest check-count-mountain-ranges
  (is (= 1 (count-mountain-ranges 5 5 5)))
  (is (= 1 (count-mountain-ranges 5 6 4)))
  (is (= 2 (count-mountain-ranges 5 7 1)))
  (is (= 3 (count-mountain-ranges 5 6 2)))
  (is (= 5 (count-mountain-ranges 5 4 4)))
  (is (= 9 (count-mountain-ranges 5 4 2)))
  (is (= 14 (count-mountain-ranges 5 2 0)))
  (is (= 28 (count-mountain-ranges 5 2 2)))
  (is (= 42 (count-mountain-ranges 5 0 0)))
  (is (= 42 (count-mountain-ranges 5 1 1)))
  (is (= 0 (count-mountain-ranges 5 10 2))))

(deftest check-unrank
  (is (match? [0 1] (unrank 1 0)))
  (is (match? [0 0 1 1] (unrank 2 0)))
  (is (match? [0 1 0 1] (unrank 2 1)))
  (is (match? [0 0 0 1 1 1] (unrank 3 0)))
  (is (match? [0 0 1 0 1 1] (unrank 3 1)))
  (is (match? [0 1 0 0 1 1] (unrank 3 2)))
  (is (match? [0 0 1 1 0 1] (unrank 3 3)))
  (is (match? [0 1 0 1 0 1] (unrank 3 4))))