(ns nullstellensatz.core
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [nullstellensatz.transaction :as transaction])
  (:gen-class))

(def cli-options
  [["-n" "--space ZERO OR POSITIVE INTEGER" "Kind of object to be generated or enumerated"
    :validate [transaction/molecule-exists? "Must be a molecule created within /data"]]
   ["-i" "--index POSITIVE INTEGER" "Structure number ID to create"
    :default 0
    :parse-fn #(Long/parseLong %)
    :validate [#(< 0 %) "Must be a number between 0 and 9223372036854775807"]]
   ["-d" "--complete-linked-diagram"]
   ["-c" "--noncrossing-linked-diagram"]
   ["-s" "--irreducible-linked-diagram"]
   ["-g" "--labeled-connected-graph"]
   ["-b" "--set-partition"]
   ["-h" "--help"]])

(defn- ->input [args]
  (parse-opts args cli-options))

(defn -main [& args]
  (-> args ->input transaction/->output))
