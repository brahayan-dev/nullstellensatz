(ns nullstellensatz.set-partition-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.set-partition :as set-partition]))

(deftest check-enumerate
  (is (= 1 (set-partition/enumerate 0)))
  (is (= 1 (set-partition/enumerate 1)))
  (is (= 2 (set-partition/enumerate 2)))
  (is (= 5 (set-partition/enumerate 3)))
  (is (= 15 (set-partition/enumerate 4)))
  (is (= 52 (set-partition/enumerate 5))))

(deftest check-encode
  (is (match? [0 0 0 0] (set-partition/encode 0 0)))
  (is (match? [1 0 0 0] (set-partition/encode 1 0)))
  (is (match? [2 0 0 0] (set-partition/encode 2 0)))
  (is (match? [2 1 0 0] (set-partition/encode 2 1)))
  (is (match? [3 0 0 0] (set-partition/encode 3 0)))
  (is (match? [3 1 0 0] (set-partition/encode 3 1)))
  (is (match? [3 1 1 0] (set-partition/encode 3 2)))
  (is (match? [3 2 0 0] (set-partition/encode 3 3)))
  (is (match? [3 2 0 1] (set-partition/encode 3 4)))
  (is (match? [4 2 0 1] (set-partition/encode 4 5)))
  (is (match? [4 3 0 3] (set-partition/encode 4 13))))

(deftest check-wrap
  (is (match? [[1 0 0 0] [2 1 0 0] [4 2 0 1]] (set-partition/wrap 4 5)))
  (is (match? [[0 0 0 0] [2 0 0 0] [3 2 0 0] [4 3 0 3]] (set-partition/wrap 4 13))))
