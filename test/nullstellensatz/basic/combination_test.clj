(ns nullstellensatz.basic.combination-test
  (:require [nullstellensatz.basic.combination :as combination]
            [matcher-combinators.test :refer [match?]]
            [clojure.test :refer [deftest is]]))

(deftest check-enumerate
  (is (= 1 (combination/enumerate 0 0)))
  (is (= 1 (combination/enumerate 3 3)))
  (is (= 5 (combination/enumerate 5 4)))
  (is (= 6 (combination/enumerate 6 1)))
  (is (= 15 (combination/enumerate 6 2)))
  (is (= 20 (combination/enumerate 6 3))))

(deftest check-generate
  (is (match? [] [])))
