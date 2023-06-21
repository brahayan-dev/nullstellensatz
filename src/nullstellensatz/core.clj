(ns nullstellensatz.core
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [nullstellensatz.transaction :as transaction])
  (:gen-class))

(def cli-options
  [["-m" "--molecule MOLECULE" "Molecule ID to load from data"
    :validate [transaction/molecule-exists? "Must be a molecule created within /data"]]
   ["-s" "--structure STRUCTURE" "Structure number ID to create"
    :default 0
    :parse-fn #(Long/parseLong %)
    :validate [#(< 0 %) "Must be a number between 0 and 9223372036854775807"]]
   ["-i" "--irreducible"]
   ["-h" "--help"]])

(defn- ->input [args]
  (parse-opts args cli-options))

(defn -main [& args]
  (-> args ->input
      transaction/load-data
      transaction/count-elements
      transaction/add-matching-size
      transaction/add-search-space-size
      transaction/validate-structure-position
      transaction/print-errors
      transaction/print-answer))
