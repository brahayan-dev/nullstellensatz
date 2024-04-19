(ns nullstellensatz.common
  (:require [schema.core :as s]))

(defn- ->names [kind]
  (case kind
    :g ["-g" "--generate-"]
    :e ["-e" "--enumerate-"]))

(defn ->options [kind letter object schema]
  (let [[short-name long-name] (->names kind)]
    [(str short-name letter)
     (str long-name object " " (quote schema))
     (s/explain schema)
     :parse-fn read-string
     :validate-msg [#(s/explain schema)]
     :validate-fn [#(s/validate schema %)]]))
