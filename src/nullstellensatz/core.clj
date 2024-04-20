(ns nullstellensatz.core
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [nullstellensatz.object.subset :as object.subset])
  (:gen-class))

(def default-options
  [["-h" "--help"]])

(def laboratory-options
  [(.option object.subset/export-generate)
   (.option object.subset/export-enumerate)])

(comment (println laboratory-options))
(comment (println
          (.id object.subset/export-enumerate)))

(defmulti reactor #(-> % keys first))

(def object-subset-export-enumerate-id (.id object.subset/export-enumerate))
(def object-subset-export-enumerate-trigger (.trigger object.subset/export-enumerate))
(defmethod reactor object-subset-export-enumerate-id [options]
  (-> options object-subset-export-enumerate-id object-subset-export-enumerate-trigger))

(def object-subset-export-generate-id (.id object.subset/export-generate))
(def object-subset-export-generate-trigger (.trigger object.subset/export-generate))
(defmethod reactor object-subset-export-generate-id [options]
  (-> options object-subset-export-generate-id object-subset-export-generate-trigger))

(defn- ->input [args]
  (->> laboratory-options
       (concat default-options)
       (into []) (parse-opts args)))

(defn- ->output [{:keys [options errors summary]}]
  (cond
    (:help options) summary
    errors (first errors)
    :else (reactor options)))

(defn -main [& args]
  (-> args ->input ->output println))
