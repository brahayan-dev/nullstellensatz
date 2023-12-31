(ns nullstellensatz.transaction-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [nullstellensatz.transaction :refer [->output]]))

(defn- ->input [options]
  {:errors []
   :summary {}
   :options options})

(deftest check->output
  (is (match? 1 (-> {:space 2
                     :object "g"} ->input ->output)))
  (is (match? [[1 2]] (-> {:space 2
                           :index 0
                           :object "g"} ->input ->output))))
