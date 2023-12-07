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
  (is (match? [3 1 0 0 0 0] (connected/unrank 3 0)))
  (is (match? [3 2 0 0 0 0] (connected/unrank 3 1)))
  (is (match? [3 2 0 1 0 0] (connected/unrank 3 2)))
  (is (match? [3 2 0 2 0 0] (connected/unrank 3 3)))

  (is (match? [4 2 1 0 0 0] (connected/unrank 4 7)))
  (is (match? [4 3 0 6 3 0] (connected/unrank 4 37)))

  (is (match? [5 2 2 0 0 1] (connected/unrank 5 63)))
  (is (match? [5 3 2 5 2 0] (connected/unrank 5 152)))
  (is (match? [5 4 0 10 14 0] (connected/unrank 5 552)))

  (is (match? [6 3 4 5 2 3] (connected/unrank 6 1723)))
  (is (match? [6 2 2 1 0 29] (connected/unrank 6 1023)))
  (is (match? [6 4 2 3 13 0] (connected/unrank 6 3123)))
  (is (match? [6 1 0 0 0 723] (connected/unrank 6 723))))
