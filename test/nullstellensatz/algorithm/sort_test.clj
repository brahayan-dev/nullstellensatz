(ns nullstellensatz.algorithm.sort-test
  (:require
   [clojure.test :refer [deftest is]]
   [nullstellensatz.algorithm.sort :as sort]))

(deftest check-sort
  (is (= (sort/quick [2 1 4 5 3]) [1 2 3 4 5]))
  (is (= (sort/standard [2 1 4 5 3]) [1 2 3 4 5])))
