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

(defn- ->input [args]
  (letfn [(connect [& opts] (into [] (concat opts)))]
    (parse-opts args (connect laboratory-options default-options))))

(defn -main [& args]
  (-> args ->input println))
