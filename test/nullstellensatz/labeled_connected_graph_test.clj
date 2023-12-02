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
