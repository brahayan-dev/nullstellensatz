(ns nullstellensatz.common
  (:require [schema.core :as s]
            [clojure.string :refer [split join]]))

(defn- ->object-keyword [k]
  (as-> k $
    (namespace $)
    (split $ #"\.")
    (drop 1 $)
    (reverse $)
    (join "-" $)))

(defn- ->flag-id [k]
  (let [process (name k)
        element (->object-keyword k)]
    (keyword (str process "-" element))))

(defn- ->cli-option [k schema]
  (let [id (-> k ->flag-id name)
        command (str "--" id " " "<SCHEMA>")
        description (-> schema s/explain str)]
    [nil command description
     :parse-fn read-string
     :validate [#(s/validate schema %)
                (str "it should be " description)]]))

(defprotocol ExportProtocol
  "A protocol for exporting functions to CLI"
  (id [this])
  (option [this])
  (callback [this]))

(defrecord Export [schema k callback]
  ExportProtocol
  (id [this] (-> this :k ->flag-id))
  (callback [this] (:callback this))
  (option [this] (->cli-option (:k this) (:schema this))))