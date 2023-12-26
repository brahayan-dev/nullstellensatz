(ns nullstellensatz.subset-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.subset :as subset]))

(deftest check-enumerate
  (is (= 4 (subset/enumerate 2)))
  (is (= 8 (subset/enumerate 3)))
  (is (= 16 (subset/enumerate 4)))
  (is (= 32 (subset/enumerate 5)))
  (is (= 64 (subset/enumerate 6))))

(deftest check-generate
  (is (match? [] (subset/generate 2 1)))
  (is (match? [1] (subset/generate 2 2)))
  (is (match? [2] (subset/generate 2 3)))
  (is (match? [1 2] (subset/generate 2 4)))
  (is (match? [] (subset/generate 3 1)))
  (is (match? [1] (subset/generate 3 2)))
  (is (match? [2] (subset/generate 3 3)))
  (is (match? [1 2] (subset/generate 3 4)))
  (is (match? [3] (subset/generate 3 5)))
  (is (match? [1 3] (subset/generate 3 6)))
  (is (match? [2 3] (subset/generate 3 7)))
  (is (match? [1 2 3] (subset/generate 3 8)))
  (is (match? [2 4] (subset/generate 4 11)))
  (is (match? [1 2 3 4] (subset/generate 4 16)))
  (is (match? [3 4 5] (subset/generate 5 29)))
  (is (match? [1 2 3 4 5] (subset/generate 5 32))))
