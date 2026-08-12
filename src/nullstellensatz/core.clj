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

(def exports
  [[object.subset/export-enumerate "subset"]
   [object.subset/export-generate "subset"]
   [object.combination/export-enumerate "combination"]
   [object.combination/export-generate "combination"]
   [object.set-partition/export-enumerate "set-partition"]
   [object.set-partition/export-generate "set-partition"]
   [object.catalan-family/export-enumerate "catalan-family"]
   [object.catalan-family/export-generate "catalan-family"]
   [object.complete-linked-diagram/export-enumerate "complete-linked-diagram"]
   [object.complete-linked-diagram/export-generate "complete-linked-diagram"]
   [object.irreducible-linked-diagram/export-enumerate "irreducible-linked-diagram"]
   [object.irreducible-linked-diagram/export-generate "irreducible-linked-diagram"]
   [object.labeled-connected-graph/export-enumerate "labeled-connected-graph"]
   [object.labeled-connected-graph/export-generate "labeled-connected-graph"]])

(doseq [[export name-suffix] exports]
  (let [id (.id export)
        callback (.callback export)
        id-sym (symbol (str "object-" name-suffix "-export-" (name (:k export)) "-id"))
        trigger-sym (symbol (str "object-" name-suffix "-export-" (name (:k export)) "-trigger"))]
    (intern *ns* id-sym id)
    (intern *ns* trigger-sym callback)
    (defmethod reactor id [options]
      (-> options id callback))))

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