(ns nullstellensatz.object.combination-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.object.combination :as combination]))

(deftest check-enumerate
  (is (= 1 (combination/enumerate 0 0)))
  (is (= 1 (combination/enumerate 3 3)))
  (is (= 5 (combination/enumerate 5 4)))
  (is (= 6 (combination/enumerate 6 1)))
  (is (= 15 (combination/enumerate 6 2)))
  (is (= 20 (combination/enumerate 6 3))))

(deftest check-generate
  (is (match? [1] (combination/generate 1 1 1)))
  (is (match? [1 2] (combination/generate 3 2 1)))
  (is (match? [1 3] (combination/generate 3 2 2)))
  (is (match? [2 3] (combination/generate 3 2 3)))
  (is (match? [1 2] (combination/generate 4 2 1)))
  (is (match? [1 3] (combination/generate 4 2 2)))
  (is (match? [2 3] (combination/generate 4 2 3)))
  (is (match? [1 4] (combination/generate 4 2 4)))
  (is (match? [2 4] (combination/generate 4 2 5)))
  (is (match? [3 4] (combination/generate 4 2 6)))
  (is (match? [5 6] (combination/generate 6 2 15)))
  (is (match? [1 2 3] (combination/generate 4 3 1)))
  (is (match? [1 2 4] (combination/generate 4 3 2)))
  (is (match? [1 3 4] (combination/generate 4 3 3)))
  (is (match? [2 3 4] (combination/generate 4 3 4)))
  (is (match? [2 4 6] (combination/generate 6 3 15)))
  (is (match? [1 2 3 4] (combination/generate 5 4 1)))
  (is (match? [1 2 3 5] (combination/generate 5 4 2)))
  (is (match? [1 2 4 5] (combination/generate 5 4 3)))
  (is (match? [1 3 4 5] (combination/generate 5 4 4)))
  (is (match? [2 3 4 5] (combination/generate 5 4 5))))
