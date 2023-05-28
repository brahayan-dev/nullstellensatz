(ns nullstellensatz.matching.irreducible-test
  (:require [clojure.test :refer [deftest is]]
            [nullstellensatz.matching.irreducible :as irreducible]))

(deftest check->code
  (is (= [0 [1 0] [2 0]] (irreducible/->code 3 0)))
  (is (= [0 [2 0] [1 0]] (irreducible/->code 3 1)))
  (is (= [1 [2 0] [1 0]] (irreducible/->code 3 2)))
  (is (= [2 [2 0] [1 0]] (irreducible/->code 3 3)))
  (is (= [0 [3 1] [2 0]] (irreducible/->code 5 40)))
  (is (= [3 [3 1] [3 2]] (irreducible/->code 6 383)))
  (is (= [3 [3 2] [4 20]] (irreducible/->code 7 3972))))

(deftest check->size
  (is (= 1 (irreducible/->size 1)))
  (is (= 1 (irreducible/->size 2)))
  (is (= 4 (irreducible/->size 3)))
  (is (= 27 (irreducible/->size 4)))
  (is (= 248 (irreducible/->size 5)))
  (is (= 2830 (irreducible/->size 6))))

(deftest check->add
  (let [m-1 [[1 2]]
        m-2 [[1 3] [2 4]]
        m-3 [[1 5] [2 4] [3 6]]
        m-4 [[1 4] [2 6] [3 5]]
        m-5 [[1 3] [2 5] [4 6]]]
    (is (= [[1 3] [2 4]] (irreducible/->add [0 m-1 m-1])))
    (is (= [[1 5] [2 4] [3 6]] (irreducible/->add [0 m-1 m-2])))
    #_(is (= [[1 4] [2 6] [3 5]] (irreducible/->add [0 m-2 m-1])))
    (is (= [[1 4] [2 5] [3 6]] (irreducible/->add [1 m-2 m-1])))
    (is (= [[1 3] [2 5] [4 6]] (irreducible/->add [2 m-2 m-1])))
    (is (= [[1 3] [2 5] [4 7] [6 8]] (irreducible/->add [4 m-5 m-1])))
    (is (= [[1 8] [2 9] [3 7] [4 6] [5 10]] (irreducible/->add [1 m-2 m-3])))
    #_(is (= [[1 8] [2 5] [3 14] [4 6] [7 10] [9 12] [11 13]]
             (irreducible/->add [0 (irreducible/->add [4 m-5 m-1]) m-4])))))

(deftest check->structure
  (is (= [[1 2]] (irreducible/->structure [0 [0 0] [0 0]])))
  (is (= [[1 3] [2 4]] (irreducible/->structure [0 [1 0] [1 0]]))))
