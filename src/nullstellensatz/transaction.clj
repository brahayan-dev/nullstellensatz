(ns nullstellensatz.transaction
  (:require
   [nullstellensatz.complete-linked-diagram :as complete-linked-diagram]
   [nullstellensatz.irreducible-linked-diagram :as irreducible-linked-diagram]
   [nullstellensatz.labeled-connected-graph :as labeled-connected-graph]
   [nullstellensatz.catalan-family :as noncrossing-linked-diagram]
   [nullstellensatz.set-partition :as set-partition]))

#_(defn- ->file [name]
    (io/file "data" (str name ".edn")))

#_(defn- load-data [{:keys [options errors] :as input}]
    (if (empty? errors)
      (let [data (-> options
                     :molecule
                     ->file io/reader
                     java.io.PushbackReader. edn/read)]
        (assoc input :data data)) input))

#_(defn- count-elements [{:keys [data errors] :as input}]
    (if (empty? errors)
      (let [raw (-> data :secondary (split #""))
            ->predicate (fn [x] #(= x %))
            ->quantity-of #(-> % ->predicate (filter raw) count)]
        (assoc-in input [:data :elements] {:free-points (->quantity-of ".")
                                           :matching-arcs (->quantity-of "(")
                                           :crossing-arcs (+
                                                           (->quantity-of "{")
                                                           (->quantity-of "["))})) input))

#_(defn- add-matching-size [{:keys [data errors] :as input}]
    (if (empty? errors)
      (letfn [(->value [{:keys [matching-arcs crossing-arcs]}]
                (if (= 0 crossing-arcs) matching-arcs (inc matching-arcs)))]
        (assoc-in input [:data :matching-size] (-> data :elements ->value))) input))

#_(defn- format [answer]
    (letfn [(->format [pair] (join "," pair))]
      (->> answer (map ->format) (join "|") println)))

#_(defn- print-answer [{:keys [errors options data]}]
    (when (empty? errors)
      (let [n (:matching-size data)
            structure (:structure options)]
        (format
         (if-not (:irreducible options)
           (complete/->structure (* 2 n) structure)
           ((comp irreducible/->structure irreducible/->code) n structure))))))

#_(defn molecule-exists? [name]
    (-> name ->file .exists))

(defn- print-answer [{:keys [size index errors]}]
  (when (and (empty? errors) (nil? index))
    (println size)))

(defn- print-errors [{:keys [errors] :as input}]
  (when-not (empty? errors)
    (-> errors first println)) input)

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

(defn- validate-index [{:keys [size index errors] :as input}]
  (if (and index (empty? errors))
    (let [error-msg (str "Failed to validate \"-m " index
                         "\": A structure must be between 0 and " size)]
      (when (> index size)
        (assoc input :errors [error-msg]))) input))

(defn- flat-data [{:keys [options errors summary]}]
  (assoc options :errors errors :summary summary))

(def ->output (comp
               print-answer
               print-errors
               validate-index
               add-size
               flat-data))
