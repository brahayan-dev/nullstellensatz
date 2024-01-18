(ns nullstellensatz.object.irreducible-linked-diagram-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.object.irreducible-linked-diagram :as irreducible]))

(deftest check-unrank
  (is (match? [2 1 0 0 0] (irreducible/unrank 2 0)))
  (is (match? [3 1 0 0 0] (irreducible/unrank 3 0)))
  (is (match? [3 2 0 0 0] (irreducible/unrank 3 1)))
  (is (match? [3 2 1 0 0] (irreducible/unrank 3 2)))
  (is (match? [3 2 2 0 0] (irreducible/unrank 3 3)))
  (is (match? [5 3 0 1 0] (irreducible/unrank 5 40)))
  (is (match? [6 3 3 1 2] (irreducible/unrank 6 383)))
  (is (match? [7 3 3 2 20] (irreducible/unrank 7 3972))))

(deftest check-enumerate
  (is (= 1 (irreducible/enumerate 1)))
  (is (= 1 (irreducible/enumerate 2)))
  (is (= 4 (irreducible/enumerate 3)))
  (is (= 27 (irreducible/enumerate 4)))
  (is (= 248 (irreducible/enumerate 5)))
  (is (= 2830 (irreducible/enumerate 6))))

(deftest check->add
  (let [m-1 [[1 2]]
        m-2 [[1 3] [2 4]]
        m-3 [[1 5] [2 4] [3 6]]
        m-4 [[1 4] [2 6] [3 5]]
        m-5 [[1 3] [2 5] [4 6]]
        ->sorted-add (comp sort irreducible/->add)]
    (is (match? [[1 3] [2 4]] (->sorted-add 0 m-1 m-1)))
    (is (match? [[1 5] [2 4] [3 6]] (->sorted-add 0 m-1 m-2)))
    (is (match? [[1 4] [2 6] [3 5]] (->sorted-add 0 m-2 m-1)))
    (is (match? [[1 4] [2 5] [3 6]] (->sorted-add 1 m-2 m-1)))
    (is (match? [[1 3] [2 5] [4 6]] (->sorted-add 2 m-2 m-1)))
    (is (match? [[1 3] [2 5] [4 7] [6 8]] (->sorted-add 4 m-5 m-1)))
    (is (match? [[1 8] [2 9] [3 7] [4 6] [5 10]] (->sorted-add 1 m-2 m-3)))
    (is (match? [[1 8] [2 5] [3 14] [4 6] [7 10] [9 12] [11 13]]
                (->sorted-add 0 (->sorted-add 4 m-5 m-1) m-4)))))

(deftest check-generate
  (is (match? [[1 2]] (irreducible/generate 1 0)))
  (is (match? [[1 3] [2 4]] (irreducible/generate 2 0)))
  (is (match? [[1 5] [2 4] [3 6]] (irreducible/generate 3 0)))
  (is (match? [[1 4] [3 5] [2 6]] (irreducible/generate 3 1)))
  (is (match? [[1 4] [2 5] [3 6]] (irreducible/generate 3 2)))
  (is (match? [[1 3] [2 5] [4 6]] (irreducible/generate 3 3))))
