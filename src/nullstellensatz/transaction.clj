(ns nullstellensatz.transaction
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn read-molecule [file]
  (with-open [stream (io/reader (str "data/" file ".edn"))]
    (-> stream java.io.PushbackReader. edn/read)))

;; (defn- path->edn [path]
;;   (letfn [(->edn [x] (json/parse-string x true))
;;           (convert [x] (map ->edn x))]
;;     (-> path io/reader line-seq convert)))

;; (defn- add-multiline-tag [{:keys [input] :as m}]
;;   (assoc m :multiline? (-> input count (> 1))))

;; (defn- ->execute-tax-calc [{:keys [input multiline?]}]
;;   (if multiline?
;;     (map domain/calculate-taxes input)
;;     (domain/calculate-taxes input)))

;; (defn compact [{:keys [multiline?] :as m} k]
;;   (letfn [(->flat [v multiline?]
;;             (if multiline? v (flatten v)))]
;;     (update m k ->flat multiline?)))

;; (defn schematize [data]
;;   (letfn [(->schema [[in out]]
;;             {:input (path->edn in)
;;              :output (path->edn out)})]
;;     (map (comp add-multiline-tag ->schema) data)))

;; (defn print-answer [items]
;;   (let [has-maps? (-> items first map?)
;;         ->print (comp println json/generate-string)]
;;     (if has-maps?
;;       (->print items)
;;       (doseq [i items] (->print i)))))

;; (defn transform [x]
;;   (let [data {:input x}
;;         ->compact #(compact % :input)]
;;     (-> data add-multiline-tag ->compact ->execute-tax-calc)))