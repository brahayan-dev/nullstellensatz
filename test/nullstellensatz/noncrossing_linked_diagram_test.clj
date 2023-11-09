(ns nullstellensatz.noncrossing-linked-diagram-test
  (:require [nullstellensatz.noncrossing-linked-diagram :refer [enumerate]]
            [clojure.test :refer [deftest is]]))

(deftest check-enumerate
  (is (= 1 (enumerate 0)))
  (is (= 1 (enumerate 1)))
  (is (= 2 (enumerate 2)))
  (is (= 5 (enumerate 3)))
  (is (= 14 (enumerate 4)))
  (is (= 42 (enumerate 5)))
  (is (= 132 (enumerate 6))))
