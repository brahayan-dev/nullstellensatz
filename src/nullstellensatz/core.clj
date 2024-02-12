(ns nullstellensatz.core
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [nullstellensatz.transaction :as transaction])
  (:gen-class))

(def cli-options
  [["-n" "--space ZERO OR POSITIVE INTEGER" "Size of object to be generated or enumerated"
    :parse-fn #(Long/parseLong %)
    :validate [#(<= 0 %) "Must be a number between 0 and 9223372036854775807"]]
   ["-m" "--index ZERO OR POSITIVE INTEGER" "Structure number ID to create"
    :parse-fn #(Long/parseLong %)
    :validate [#(<= 0 %) "Must be a number between 0 and 9223372036854775807"]]
   ["-k" "--fixed-size ZERO OR POSITIVE INTEGER" "Fixed size of subset to be generated or enumerated in combinations (f)"
    :default 1
    :parse-fn #(Long/parseLong %)
    :validate [#(<= 0 %) "Must be a number between 0 and 9223372036854775807"]]
   ["-o" "--object ONE OPTION OF: 'a', 'f', 'b', 'c', 'd', 'g' or 's'" "Type of object to be generated or enumerated"
    :validate [#(some #{%} ["a" "f" "b" "c" "d" "g" "s"]) "Must be selected one of: 'a', 'f', 'b', 'c', 'd', 'g' or 's'"]]
   ["-r" "--is-randomized"]
   ["-h" "--help"]])

(defn- ->input [args]
  (parse-opts args cli-options))

(defn -main [& args]
  (-> args ->input transaction/->output println))
