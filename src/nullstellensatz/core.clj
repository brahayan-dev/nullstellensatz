(ns nullstellensatz.core
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.tools.cli :refer [parse-opts]]
   [nullstellensatz.transaction :as transaction])
  (:gen-class))

(def cli-options
  [["-m" "--molecule MOLECULE" "Molecule ID to load from data"
    :validate [transaction/molecule-exists? "Must be a molecule created within /data"]]
   ["-s" "--structure STRUCTURE" "Structure number ID to create"
    :default 0
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-i" "--irreducible"]
   ["-h" "--help"]])

(defn- ->input [args]
  (parse-opts args cli-options))

(defn -main [& args]
  (-> args ->input
      transaction/print-errors 
      transaction/read-data pprint))
