(ns nullstellensatz.core
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [nullstellensatz.object.subset :as subset])
  (:gen-class))

(def laboratory-options
  [subset/generate-options
   subset/enumerate-options])

(def default-options
  [["-r" "--randomized"]
   ["-h" "--help"]])

(defn ->input [args]
  (->> laboratory-options
       (concat default-options)
       (into []) (parse-opts args)))

(defn -main [& args]
  (-> args ->input println))
