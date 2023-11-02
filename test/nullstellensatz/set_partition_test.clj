(ns nullstellensatz.set-partition-test
  (:require [nullstellensatz.set-partition :as set-partition]
[clojure.test :refer [testing is deftest]]            ))

(deftest name-test
      (testing "Context of the test assertions"
        (is (= 1 (set-partition/enumerate 1))))) 

