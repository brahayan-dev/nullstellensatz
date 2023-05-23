(ns nullstellensatz.core
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def cli-options
  [["-m" "--molecule MOLECULE" "Molecule ID to load from data"
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-s" "--structure STRUCTURE" "Structure number ID to create"
    :default 0
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-i" "--irreducible"]
   ["-h" "--help"]])

(defn -main [& args]
  (println (parse-opts args cli-options)))
