(ns nullstellensatz.core
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [nullstellensatz.object.subset :as object.subset]
   [nullstellensatz.object.combination :as object.combination]
   [nullstellensatz.object.set-partition :as object.set-partition]
   [nullstellensatz.object.catalan-family :as object.catalan-family]
   [nullstellensatz.object.complete-linked-diagram :as object.complete-linked-diagram]
   [nullstellensatz.object.labeled-connected-graph :as object.labeled-connected-graph]
   [nullstellensatz.object.irreducible-linked-diagram :as object.irreducible-linked-diagram])
  (:gen-class))

(def default-options
  [["-h" "--help"]])

(def laboratory-options
  [(.option object.subset/export-generate)
   (.option object.subset/export-enumerate)
   (.option object.combination/export-generate)
   (.option object.combination/export-enumerate)
   (.option object.set-partition/export-generate)
   (.option object.set-partition/export-enumerate)
   (.option object.catalan-family/export-generate)
   (.option object.catalan-family/export-enumerate)
   (.option object.labeled-connected-graph/export-generate)
   (.option object.labeled-connected-graph/export-enumerate)
   (.option object.complete-linked-diagram/export-generate)
   (.option object.complete-linked-diagram/export-enumerate)
   (.option object.irreducible-linked-diagram/export-generate)
   (.option object.irreducible-linked-diagram/export-enumerate)])

(defmulti reactor #(-> % keys first))

(def object-subset-export-enumerate-id (.id object.subset/export-enumerate))
(def object-subset-export-enumerate-trigger (.trigger object.subset/export-enumerate))
(defmethod reactor object-subset-export-enumerate-id [options]
  (-> options object-subset-export-enumerate-id object-subset-export-enumerate-trigger))

(def object-subset-export-generate-id (.id object.subset/export-generate))
(def object-subset-export-generate-trigger (.trigger object.subset/export-generate))
(defmethod reactor object-subset-export-generate-id [options]
  (-> options object-subset-export-generate-id object-subset-export-generate-trigger))

(def object-combination-export-enumerate-id (.id object.combination/export-enumerate))
(def object-combination-export-enumerate-trigger (.trigger object.combination/export-enumerate))
(defmethod reactor object-combination-export-enumerate-id [options]
  (-> options object-combination-export-enumerate-id object-combination-export-enumerate-trigger))

(def object-combination-export-generate-id (.id object.combination/export-generate))
(def object-combination-export-generate-trigger (.trigger object.combination/export-generate))
(defmethod reactor object-combination-export-generate-id [options]
  (-> options object-combination-export-generate-id object-combination-export-generate-trigger))

(def object-set-partition-export-enumerate-id (.id object.set-partition/export-enumerate))
(def object-set-partition-export-enumerate-trigger (.trigger object.set-partition/export-enumerate))
(defmethod reactor object-set-partition-export-enumerate-id [options]
  (-> options object-set-partition-export-enumerate-id object-set-partition-export-enumerate-trigger))

(def object-set-partition-export-generate-id (.id object.set-partition/export-generate))
(def object-set-partition-export-generate-trigger (.trigger object.set-partition/export-generate))
(defmethod reactor object-set-partition-export-generate-id [options]
  (-> options object-set-partition-export-generate-id object-set-partition-export-generate-trigger))

(def object-catalan-family-export-enumerate-id (.id object.catalan-family/export-enumerate))
(def object-catalan-family-export-enumerate-trigger (.trigger object.catalan-family/export-enumerate))
(defmethod reactor object-catalan-family-export-enumerate-id [options]
  (-> options object-catalan-family-export-enumerate-id object-catalan-family-export-enumerate-trigger))

(def object-catalan-family-export-generate-id (.id object.catalan-family/export-generate))
(def object-catalan-family-export-generate-trigger (.trigger object.catalan-family/export-generate))
(defmethod reactor object-catalan-family-export-generate-id [options]
  (-> options object-catalan-family-export-generate-id object-catalan-family-export-generate-trigger))

(def object-complete-linked-diagram-export-enumerate-id (.id object.complete-linked-diagram/export-enumerate))
(def object-complete-linked-diagram-export-enumerate-trigger (.trigger object.complete-linked-diagram/export-enumerate))
(defmethod reactor object-complete-linked-diagram-export-enumerate-id [options]
  (-> options object-complete-linked-diagram-export-enumerate-id object-complete-linked-diagram-export-enumerate-trigger))

(def object-complete-linked-diagram-export-generate-id (.id object.complete-linked-diagram/export-generate))
(def object-complete-linked-diagram-export-generate-trigger (.trigger object.complete-linked-diagram/export-generate))
(defmethod reactor object-complete-linked-diagram-export-generate-id [options]
  (-> options object-complete-linked-diagram-export-generate-id object-complete-linked-diagram-export-generate-trigger))

(def object-irreducible-linked-diagram-export-enumerate-id (.id object.irreducible-linked-diagram/export-enumerate))
(def object-irreducible-linked-diagram-export-enumerate-trigger (.trigger object.irreducible-linked-diagram/export-enumerate))
(defmethod reactor object-irreducible-linked-diagram-export-enumerate-id [options]
  (-> options object-irreducible-linked-diagram-export-enumerate-id object-irreducible-linked-diagram-export-enumerate-trigger))

(def object-irreducible-linked-diagram-export-generate-id (.id object.irreducible-linked-diagram/export-generate))
(def object-irreducible-linked-diagram-export-generate-trigger (.trigger object.irreducible-linked-diagram/export-generate))
(defmethod reactor object-irreducible-linked-diagram-export-generate-id [options]
  (-> options object-irreducible-linked-diagram-export-generate-id object-irreducible-linked-diagram-export-generate-trigger))

(def object-labeled-connected-graph-export-enumerate-id (.id object.labeled-connected-graph/export-enumerate))
(def object-labeled-connected-graph-export-enumerate-trigger (.trigger object.labeled-connected-graph/export-enumerate))
(defmethod reactor object-labeled-connected-graph-export-enumerate-id [options]
  (-> options object-labeled-connected-graph-export-enumerate-id object-labeled-connected-graph-export-enumerate-trigger))

(def object-labeled-connected-graph-export-generate-id (.id object.labeled-connected-graph/export-generate))
(def object-labeled-connected-graph-export-generate-trigger (.trigger object.labeled-connected-graph/export-generate))
(defmethod reactor object-labeled-connected-graph-export-generate-id [options]
  (-> options object-labeled-connected-graph-export-generate-id object-labeled-connected-graph-export-generate-trigger))

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
