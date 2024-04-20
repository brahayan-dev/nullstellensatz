(ns nullstellensatz.common
  (:require [schema.core :as s]
            [clojure.string :refer [split join]]))

(defn- ->element [k]
  (as-> k $
    (namespace $)
    (split $ #"\.")
    (drop 1 $)
    (reverse $)
    (join "-" $)))

(defn- ->id [k]
  (let [process (name k)
        element (->element k)]
    (keyword (str process "-" element))))

(defn- ->option [k schema]
  (let [id (-> k ->id name)
        command (str "--" id " " "SCHEMA")
        description (-> schema s/explain str)]
    [nil command description
     :parse-fn read-string
     :validate [#(s/validate schema %)
                (str "it should be " description)]]))

(defprotocol ExportProtocol
  "A protocol for exporting functions to CLI"
  (id [this])
  (option [this])
  (trigger [this]))

(defrecord Export [schema k callback]
  ExportProtocol
  (id [this] (-> this :k ->id))
  (trigger [this] (:callback this))
  (option [this] (->option (:k this) (:schema this))))
