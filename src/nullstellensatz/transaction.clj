(ns nullstellensatz.transaction
  (:require
   [nullstellensatz.object.catalan-family :as noncrossing-linked-diagram]
   [nullstellensatz.object.complete-linked-diagram :as complete-linked-diagram]
   [nullstellensatz.object.irreducible-linked-diagram :as irreducible-linked-diagram]
   [nullstellensatz.object.labeled-connected-graph :as labeled-connected-graph]
   [nullstellensatz.object.set-partition :as set-partition]))

#_(defn- ->file [name]
    (io/file "data" (str name ".edn")))

(defn- ->answer [{:keys [generated size index]}]
  (when size (if index generated size)))

(defn- ->errors [{:keys [errors] :as input}]
  (when-not (empty? errors)
    (first errors)) input)

(defn- add-size [{:keys [space object errors] :as input}]
  (if (empty? errors)
    (let [n space
          size (case object
                 "b" (set-partition/enumerate n)
                 "g" (labeled-connected-graph/enumerate n)
                 "d" (complete-linked-diagram/enumerate n)
                 "c" (noncrossing-linked-diagram/enumerate n)
                 "s" (irreducible-linked-diagram/enumerate n))]
      (assoc input :size size)) input))

(defn- fetch-object [{:keys [index object space errors] :as input}]
  (if (and index (empty? errors))
    (let [n space
          generated (case object
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

(defn- flat-data [{:keys [options errors summary]}]
  (assoc options :errors errors :summary summary))

(def ->output (comp
               ->answer
               ->errors
               fetch-object
               validate-index
               add-size
               flat-data))
