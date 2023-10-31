(ns nullstellensatz.labeled-connected-graph-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.labeled-connected-graph :as connected]))

(def polynomials
  {1 [{:n-value 1 :k-value 1 :n-k-value 0 :subset-value 1 :binomial-value 1}]
   2 [{:n-value 2 :k-value 1 :n-k-value 1 :subset-value 1 :binomial-value 1}]
   3 [{:n-value 3 :k-value 1 :n-k-value 2 :subset-value 1 :binomial-value 1}
      {:n-value 3 :k-value 2 :n-k-value 1 :subset-value 3 :binomial-value 1}]
   4 [{:n-value 4 :k-value 1 :n-k-value 3 :subset-value 1 :binomial-value 1}
      {:n-value 4 :k-value 2 :n-k-value 2 :subset-value 3 :binomial-value 2}
      {:n-value 4 :k-value 3 :n-k-value 1 :subset-value 7 :binomial-value 1}]
   5 [{:n-value 5 :k-value 1 :n-k-value 4 :subset-value 1 :binomial-value 1}
      {:n-value 5 :k-value 2 :n-k-value 3 :subset-value 3 :binomial-value 3}
      {:n-value 5 :k-value 3 :n-k-value 2 :subset-value 7 :binomial-value 3}
      {:n-value 5 :k-value 4 :n-k-value 1 :subset-value 15 :binomial-value 1}]})

(def results
  "[[1 1] 1
    [2 1] 1
    [3 1] 1  [3 2] 3
    [4 1] 4  [4 2] 6  [4 3] 28
    [5 1] 38 [5 2] 36 [5 3] 84 [5 4] 570]"
  {1 [1]
   2 [1 1]
   3 [1 1 1 3]
   4 [1 1 1 3 4 6 28]
   5 [1 1 1 3 4 6 28 38 36 84 570]})

(deftest check->items
  (is (match? [1] (connected/->items 1)))
  (is (match? [1] (connected/->items 2)))
  (is (match? [1 2] (connected/->items 3)))
  (is (match? [1 2 3] (connected/->items 4)))
  (is (match? [1 2 3 4] (connected/->items 5)))
  (is (match? [1 2 3 4 5] (connected/->items 6))))

(deftest check->packs
  (is (match? (get polynomials 1) (connected/->packs 1)))
  (is (match? (get polynomials 2) (connected/->packs 2)))
  (is (match? (get polynomials 3) (connected/->packs 3)))
  (is (match? (get polynomials 4) (connected/->packs 4)))
  (is (match? (get polynomials 5) (connected/->packs 5))))

(deftest check->quantities
  (is (match? (get results 1) (connected/->quantities 1)))
  (is (match? (get results 2) (connected/->quantities 2)))
  (is (match? (get results 3) (connected/->quantities 3)))
  (is (match? (get results 4) (connected/->quantities 4)))
  (is (match? (get results 5) (connected/->quantities 5))))

(deftest check-enumerate
  (is (= 1 (connected/enumerate 1)))
  (is (= 1 (connected/enumerate 2)))
  (is (= 4 (connected/enumerate 3)))
  (is (= 38 (connected/enumerate 4)))
  (is (= 728 (connected/enumerate 5)))
  (is (= 26704 (connected/enumerate 6))))

