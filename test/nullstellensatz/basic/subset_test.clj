(ns nullstellensatz.basic.subset-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.basic.subset :as subset]))

(deftest check-enumerate
  (is (= 4 (subset/enumerate 2)))
  (is (= 8 (subset/enumerate 3)))
  (is (= 16 (subset/enumerate 4)))
  (is (= 32 (subset/enumerate 5)))
  (is (= 64 (subset/enumerate 6))))

(deftest check-search
  (is (match? [0 0 0] (subset/search 3 1)))
  (is (match? [0 0 1] (subset/search 3 2)))
  (is (match? [0 1 0] (subset/search 3 3)))
  (is (match? [0 1 1] (subset/search 3 4)))
  (is (match? [1 0 0] (subset/search 3 5)))
  (is (match? [1 0 1] (subset/search 3 6)))
  (is (match? [1 1 0] (subset/search 3 7)))
  (is (match? [1 1 1] (subset/search 3 8)))
  (is (match? [1 1 1 0 0] (subset/search 5 29))))

(deftest check-generate
  (is (match? [] (subset/generate 3 1)))
  (is (match? [3] (subset/generate 3 2)))
  (is (match? [2] (subset/generate 3 3)))
  (is (match? [2 3] (subset/generate 3 4)))
  (is (match? [1] (subset/generate 3 5)))
  (is (match? [1 3] (subset/generate 3 6)))
  (is (match? [1 2] (subset/generate 3 7)))
  (is (match? [1 2 3] (subset/generate 3 8)))
  (is (match? [1 2 3] (subset/generate 5 29))))
