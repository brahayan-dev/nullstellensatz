(ns nullstellensatz.transaction
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :refer [split join]]
            [nullstellensatz.complete-linked-diagram :as complete]
            [nullstellensatz.irreducible-linked-diagram :as irreducible]))

(defn- ->file [name]
  (io/file "data" (str name ".edn")))

(defn- print-errors [{:keys [errors] :as input}]
  (when-not (empty? errors)
    (-> errors first println)) input)

(defn- load-data [{:keys [options errors] :as input}]
  (if (empty? errors)
    (let [data (-> options
                   :molecule
                   ->file io/reader
                   java.io.PushbackReader. edn/read)]
      (assoc input :data data)) input))

(defn- count-elements [{:keys [data errors] :as input}]
  (if (empty? errors)
    (let [raw (-> data :secondary (split #""))
          ->predicate (fn [x] #(= x %))
          ->quantity-of #(-> % ->predicate (filter raw) count)]
      (assoc-in input [:data :elements] {:free-points (->quantity-of ".")
                                         :matching-arcs (->quantity-of "(")
                                         :crossing-arcs (+
                                                         (->quantity-of "{")
                                                         (->quantity-of "["))})) input))

(defn- add-matching-size [{:keys [data errors] :as input}]
  (if (empty? errors)
    (letfn [(->value [{:keys [matching-arcs crossing-arcs]}]
              (if (= 0 crossing-arcs) matching-arcs (inc matching-arcs)))]
      (assoc-in input [:data :matching-size] (-> data :elements ->value))) input))

(defn- add-search-space-size [{:keys [data options errors] :as input}]
  (if (empty? errors)
    (let [n (:matching-size data)
          size (if (:irreducible options)
                 (irreducible/->size n)
                 (complete/->size (* 2 n)))]
      (assoc-in input [:data :search-space-size] size)) input))

(defn- validate-structure-position [{:keys [data options errors] :as input}]
  (if (empty? errors)
    (let [structure (:structure options)
          space-size (:search-space-size data)
          error-msg (str "Failed to validate \"-s " structure
                         "\": A structure must be between 0 and " space-size)]
      (if (> structure space-size)
        (assoc input :errors [error-msg]) input)) input))

(defn- format-matching [answer]
  (letfn [(->format [pair] (join "," pair))]
    (->> answer (map ->format) (join "|") println)))

(defn- print-answer [{:keys [errors options data]}]
  (when (empty? errors)
    (let [n (:matching-size data)
          structure (:structure options)]
      (format-matching
       (if-not (:irreducible options)
         (complete/->structure (* 2 n) structure)
         ((comp irreducible/->structure irreducible/->code) n structure))))))

(defn molecule-exists? [name]
  (-> name ->file .exists))

(def ->output (comp
               print-answer
               print-errors
               validate-structure-position
               add-search-space-size
               add-matching-size
               count-elements
               load-data))
