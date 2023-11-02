(ns nullstellensatz.set-partition-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.set-partition :as set-partition]))

(deftest check-enumerate
  (is (= 1 (set-partition/enumerate 0)))
  (is (= 1 (set-partition/enumerate 1)))
  (is (= 2 (set-partition/enumerate 2)))
  (is (= 5 (set-partition/enumerate 3)))
  (is (= 15 (set-partition/enumerate 4)))
  (is (= 52 (set-partition/enumerate 5))))

(deftest check-encode
  (is (match? [4 2 0 1] (set-partition/encode 4 5)))
  (is (match? [4 3 0 3] (set-partition/encode 4 13))))
