(ns nullstellensatz.labeled-connected-graph-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.labeled-connected-graph :as connected]))

(deftest check-enumerate
  (is (= 1 (connected/enumerate 1)))
  (is (= 1 (connected/enumerate 2)))
  (is (= 4 (connected/enumerate 3)))
  (is (= 38 (connected/enumerate 4)))
  (is (= 728 (connected/enumerate 5)))
  (is (= 26704 (connected/enumerate 6))))

(deftest check-unrank
  (is (match? [1 1 0 0 0 0] (connected/unrank 1 0)))
  (is (match? [2 1 0 0 0 0] (connected/unrank 2 0)))

  (is (match? [3 1 0 0 0 0] (connected/unrank 3 0)))
  (is (match? [3 2 0 0 0 0] (connected/unrank 3 1)))
  (is (match? [3 2 0 1 0 0] (connected/unrank 3 2)))
  (is (match? [3 2 0 2 0 0] (connected/unrank 3 3)))

  (is (match? [4 2 0 0 0 0] (connected/unrank 4 4)))
  (is (match? [4 2 1 0 0 0] (connected/unrank 4 7)))
  (is (match? [4 3 0 4 3 0] (connected/unrank 4 29)))
  (is (match? [4 3 0 6 3 0] (connected/unrank 4 37)))

  (is (match? [5 2 2 0 0 1] (connected/unrank 5 63)))
  (is (match? [5 3 2 5 2 0] (connected/unrank 5 152)))
  (is (match? [5 4 0 10 14 0] (connected/unrank 5 552)))

  (is (match? [6 3 4 5 2 3] (connected/unrank 6 1723)))
  (is (match? [6 2 2 1 0 29] (connected/unrank 6 1023)))
  (is (match? [6 4 2 3 13 0] (connected/unrank 6 3123)))
  (is (match? [6 1 0 0 0 723] (connected/unrank 6 723)))

  (is (match? [7 3 2 2 1 29] (connected/unrank 7 40123)))
  (is (match? [8 4 12 10 4 4] (connected/unrank 8 2927204))))

(deftest check-unwrap
  (is (match? [[1 1 0 0 0 0]] (connected/unwrap 1 0)))
  (is (match? [[1 1 0 0 0 0] [2 1 0 0 0 0]] (connected/unwrap 2 0)))
  (is (match? [[1 1 0 0 0 0] [2 1 0 0 0 0] [3 1 0 0 0 0]] (connected/unwrap 3 0)))
  (is (match? [[1 1 0 0 0 0] [2 1 0 0 0 0] [3 2 0 0 0 0]] (connected/unwrap 3 1)))
  (is (match? [[1 1 0 0 0 0] [2 1 0 0 0 0] [3 2 0 1 0 0]] (connected/unwrap 3 2)))
  (is (match? [[1 1 0 0 0 0] [2 1 0 0 0 0] [3 2 0 2 0 0]] (connected/unwrap 3 3))))

(deftest check-relabel
  (is (match? [[4]] (connected/relabel [[1]] [4])))
  (is (match? [[4 5]] (connected/relabel [[1 2]] [4 5])))
  (is (match? [[4 5] [5 6]] (connected/relabel [[1 2] [2 3]] [4 5 6])))
  (is (match? [[4 5] [4 6] [5 6]] (connected/relabel [[1 2] [1 3] [2 3]] [4 5 6]))))

(def ^:private cache
  {1 [[1]]
   2 [[1 2]]})

(deftest check-compact
  (is (match? [3 1 [1] [1] [[1]] [[1 2]]] (connected/compact cache [3 1 0 0 0 0])))
  (is (match? [3 2 [1 2] [1] [[1 2]] [[1]]] (connected/compact cache [3 2 0 0 0 0])))
  (is (match? [3 2 [1 2] [2] [[1 2]] [[1]]] (connected/compact cache [3 2 0 1 0 0])))
  (is (match? [3 2 [1 2] [1 2] [[1 2]] [[1]]] (connected/compact cache [3 2 0 2 0 0])))

  (is (match? [4 1 [1] [1] [[1]] [[1 2] [2 3]]]
              (connected/compact (assoc cache 3 [[1 2] [2 3]]) [4 1 0 0 0 2]))))

(deftest check-generate
  (is (match? [[1]] (connected/generate 1 0)))
  (is (match? [[1 2]] (connected/generate 2 0)))
  (is (match? [[2 3] [1 3]] (connected/generate 3 0)))
  (is (match? [[1 2] [1 3]] (connected/generate 3 1)))
  (is (match? [[1 2] [2 3]] (connected/generate 3 2)))
  (is (match? [[1 2] [1 3] [2 3]] (connected/generate 3 3)))
  (is (match? [[3 4] [2 4] [1 4]] (connected/generate 4 0)))
  (is (match? [[2 3] [3 4] [1 4]] (connected/generate 4 2)))
  (is (match? [[1 2] [3 4] [2 4]] (connected/generate 4 5)))
  (is (match? [[1 3] [2 4] [2 4]] (connected/generate 4 8)))
  (is (match? [[1 2] [1 3] [2 3] [3 4]] (connected/generate 4 25)))
  (is (match? [[1 2] [1 3] [2 4] [3 4]] (connected/generate 4 31)))
  (is (match? [[1 2] [2 3] [1 4] [2 4] [3 4]] (connected/generate 4 36))))
