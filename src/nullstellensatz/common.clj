(ns nullstellensatz.common
  (:require [schema.core :as s]
            [clojure.string :refer [split join]]))

(defn- ->object [id]
  (as-> id $
    (namespace $)
    (split $ #"\.")
    (drop 1 $)
    (reverse $)
    (join "-" $)))

(defn ->options [id schema]
  (let [process (name id)
        object (->object id)
        description (-> schema s/explain str)
        command (str "--" process "-" object " " "SCHEMA")]
    (println id)
    [nil command description
     :parse-fn read-string
     :validate [#(s/validate schema %) description]]))
