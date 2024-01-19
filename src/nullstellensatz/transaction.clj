(ns nullstellensatz.transaction
  (:require
   [nullstellensatz.object.catalan-family :as noncrossing-linked-diagram]
   [nullstellensatz.object.combination :as combination]
   [nullstellensatz.object.complete-linked-diagram :as complete-linked-diagram]
   [nullstellensatz.object.irreducible-linked-diagram :as irreducible-linked-diagram]
   [nullstellensatz.object.labeled-connected-graph :as labeled-connected-graph]
   [nullstellensatz.object.set-partition :as set-partition]
   [nullstellensatz.object.subset :as subset]))

(defn- ->answer [{:keys [generated size index]}]
  (when size (if index generated size)))

(defn- ->errors [{:keys [errors] :as input}]
  (when-not (empty? errors)
    (first errors)) input)

(defn- add-size [{:keys [fixed-size space object errors] :as input}]
  (if (empty? errors)
    (let [n space
          k fixed-size
          size (case object
                 "a" (subset/enumerate n)
                 "f" (combination/enumerate n k)
                 "b" (set-partition/enumerate n)
                 "g" (labeled-connected-graph/enumerate n)
                 "d" (complete-linked-diagram/enumerate n)
                 "c" (noncrossing-linked-diagram/enumerate n)
                 "s" (irreducible-linked-diagram/enumerate n))]
      (assoc input :size size)) input))

(defn- fetch-object [{:keys [fixed-size index object space errors] :as input}]
  (if (and index (empty? errors))
    (let [n space
          k fixed-size
          generated (case object
                      "a" (subset/generate n)
                      "f" (combination/generate n k index)
                      "b" (set-partition/generate n index)
                      "g" (labeled-connected-graph/generate n index)
                      "d" (complete-linked-diagram/generate n index)
                      "c" (noncrossing-linked-diagram/generate n index)
                      "s" (irreducible-linked-diagram/generate n index))]
      (assoc input :generated generated)) input))

(defn- validate-index [{:keys [size index errors] :as input}]
  (if (and index (> index size) (empty? errors))
    (let [error-msg (str "Failed to validate \"-m " index
                         "\": A structure must be between 0 and " size)]
      (assoc input :errors [error-msg])) input))

(defn- validate-fixed-size [{:keys [size index errors fixed-size] :as input}]
  (if (and fixed-size (> fixed-size size) (empty? errors))
    (let [error-msg (str "Failed to validate \"-k " index
                         "\": A fixed k-subset must be between 0 and " size)]
      (assoc input :errors [error-msg])) input))

(defn- flat-data [{:keys [options errors summary]}]
  (assoc options :errors errors :summary summary))

(def ->output (comp
               ->answer
               ->errors
               fetch-object
               validate-index
               validate-fixed-size
               add-size
               flat-data))
