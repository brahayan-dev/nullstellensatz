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

(defn ->identifier [k]
  (let [process (name k)
        element (->element k)]
    (keyword (str process "-" element))))

(defn ->options [k schema]
  (let [id (-> k ->identifier name)
        command (str "--" id " " "SCHEMA")
        description (-> schema s/explain str)]
    [nil command description
     :parse-fn read-string
     :validate [#(s/validate schema %)
                (str "it should have this SCHEMA " description)]]))
