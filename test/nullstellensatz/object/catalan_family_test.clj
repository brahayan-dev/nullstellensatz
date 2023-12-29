(ns nullstellensatz.object.catalan-family-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.object.catalan-family :as catalan]))

(deftest check-enumerate
  (is (= 1 (catalan/enumerate 0)))
  (is (= 1 (catalan/enumerate 1)))
  (is (= 2 (catalan/enumerate 2)))
  (is (= 5 (catalan/enumerate 3)))
  (is (= 14 (catalan/enumerate 4)))
  (is (= 42 (catalan/enumerate 5)))
  (is (= 132 (catalan/enumerate 6))))

(deftest check-count-mountain-ranges
  (is (= 1 (catalan/count-mountain-ranges 5 5 5)))
  (is (= 1 (catalan/count-mountain-ranges 5 6 4)))
  (is (= 2 (catalan/count-mountain-ranges 5 7 1)))
  (is (= 3 (catalan/count-mountain-ranges 5 6 2)))
  (is (= 5 (catalan/count-mountain-ranges 5 4 4)))
  (is (= 9 (catalan/count-mountain-ranges 5 4 2)))
  (is (= 14 (catalan/count-mountain-ranges 5 2 0)))
  (is (= 28 (catalan/count-mountain-ranges 5 2 2)))
  (is (= 42 (catalan/count-mountain-ranges 5 0 0)))
  (is (= 42 (catalan/count-mountain-ranges 5 1 1)))
  (is (= 0 (catalan/count-mountain-ranges 5 10 2))))

(deftest check-unrank
  (is (match? [0 1] (catalan/unrank 1 0)))
  (is (match? [0 0 1 1] (catalan/unrank 2 0)))
  (is (match? [0 1 0 1] (catalan/unrank 2 1)))
  (is (match? [0 0 0 1 1 1] (catalan/unrank 3 0)))
  (is (match? [0 0 1 0 1 1] (catalan/unrank 3 1)))
  (is (match? [0 1 0 0 1 1] (catalan/unrank 3 2)))
  (is (match? [0 0 1 1 0 1] (catalan/unrank 3 3)))
  (is (match? [0 1 0 1 0 1] (catalan/unrank 3 4))))

(deftest check-search-dyck-path
  (is (match? [[0 0] [1 1] [2 2] [3 1] [4 0]] (catalan/generate 2 0)))
  (is (match? [[0 0] [1 1] [2 0] [3 1] [4 0]] (catalan/generate 2 1)))
  (is (match? [[0 0] [1 1] [2 2] [3 3] [4 2] [5 1] [6 0]] (catalan/generate 3 0)))
  (is (match? [[0 0] [1 1] [2 2] [3 1] [4 2] [5 1] [6 0]] (catalan/generate 3 1)))
  (is (match? [[0 0] [1 1] [2 0] [3 1] [4 2] [5 1] [6 0]] (catalan/generate 3 2)))
  (is (match? [[0 0] [1 1] [2 2] [3 1] [4 0] [5 1] [6 0]] (catalan/generate 3 3)))
  (is (match? [[0 0] [1 1] [2 0] [3 1] [4 0] [5 1] [6 0]] (catalan/generate 3 4))))
