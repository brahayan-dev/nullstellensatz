(ns nullstellensatz.object.set-partition-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.object.set-partition :as set-partition]))

(deftest check-enumerate
  (is (= 1 (set-partition/enumerate 0)))
  (is (= 1 (set-partition/enumerate 1)))
  (is (= 2 (set-partition/enumerate 2)))
  (is (= 5 (set-partition/enumerate 3)))
  (is (= 15 (set-partition/enumerate 4)))
  (is (= 52 (set-partition/enumerate 5))))

(deftest check-unrank
  (is (match? {1 1 2 1 3 1 4 1} (set-partition/unrank 4 0)))
  (is (match? {1 1 2 1 3 1 4 2} (set-partition/unrank 4 1)))
  (is (match? {1 1 2 1 3 2 4 1} (set-partition/unrank 4 2)))
  (is (match? {1 1 2 1 3 2 4 2} (set-partition/unrank 4 3)))
  (is (match? {1 1 2 1 3 2 4 3} (set-partition/unrank 4 4)))
  (is (match? {1 1 2 2 3 1 4 1} (set-partition/unrank 4 5)))
  (is (match? {1 1 2 2 3 1 4 2} (set-partition/unrank 4 6)))
  (is (match? {1 1 2 2 3 1 4 3} (set-partition/unrank 4 7)))
  (is (match? {1 1 2 2 3 2 4 1} (set-partition/unrank 4 8)))
  (is (match? {1 1 2 2 3 2 4 2} (set-partition/unrank 4 9)))
  (is (match? {1 1 2 2 3 2 4 3} (set-partition/unrank 4 10)))
  (is (match? {1 1 2 2 3 3 4 1} (set-partition/unrank 4 11)))
  (is (match? {1 1 2 2 3 3 4 2} (set-partition/unrank 4 12)))
  (is (match? {1 1 2 2 3 3 4 3} (set-partition/unrank 4 13)))
  (is (match? {1 1 2 2 3 3 4 4} (set-partition/unrank 4 14))))

(deftest check-search
  (is (match? [[1 2 3]] (set-partition/generate 3 0)))
  (is (match? [[1 2] [3]] (set-partition/generate 3 1)))
  (is (match? [[1 3] [2]] (set-partition/generate 3 2)))
  (is (match? [[1] [2 3]] (set-partition/generate 3 3)))
  (is (match? [[1] [2] [3]] (set-partition/generate 3 4)))
  (is (match? [[1 3 4] [2]] (set-partition/generate 4 5)))
  (is (match? [[1] [2] [3 4]] (set-partition/generate 4 13))))
