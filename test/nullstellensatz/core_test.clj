(ns nullstellensatz.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.core :refer [->input]]))

(deftest check->input
  (is (match? 1 (->input "(-ea {:n 2})"))))

#_(deftest check->output
    (is (match? 1 (-> {:space 2
                       :object "g"} ->input ->output)))
    (is (match? [[1 2]] (-> {:space 2
                             :index 0
                             :object "g"} ->input ->output)))
    (is (match? 8 (-> {:space 3
                       :object "a"} ->input ->output)))
    (is (match? [1 3] (-> {:space 3
                           :index 6
                           :object "a"} ->input ->output)))
    (is (match? 3 (-> {:space 3
                       :fixed-size 2
                       :object "f"} ->input ->output)))
    (is (match? 3 (-> {:space 3
                       :fixed-size 1
                       :object "f"} ->input ->output)))
    (is (match? [4] (-> {:space 4
                         :index 4
                         :fixed-size 1
                         :object "f"} ->input ->output)))
    (is (match? [2] (-> {:space 2
                         :index 2
                         :fixed-size 1
                         :object "f"} ->input ->output))))
