(ns nullstellensatz.core
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [nullstellensatz.object.subset :as subset])
  (:gen-class))

(def default-options
  [["-h" "--help"]
   ["-r" "--randomized"]])

(def laboratory-options
  [subset/generate-options
   subset/enumerate-options])

(defmulti reactor #(-> % keys first))

(defmethod reactor subset/generate-id [options]
  (-> options subset/generate-id subset/->generated))

(defmethod reactor subset/enumerate-id [options]
  (-> options subset/enumerate-id subset/->enumerated))

(defn- ->input [args]
  (->> laboratory-options
       (concat default-options)
       (into []) (parse-opts args)))

(defn- ->output [{:keys [options errors summary]}]
  (if (:help options) summary
      (or errors (reactor options))))

(defn -main [& args]
  (-> args ->input ->output println))
