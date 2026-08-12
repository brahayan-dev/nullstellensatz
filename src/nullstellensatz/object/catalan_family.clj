(ns nullstellensatz.object.catalan-family
  (:require [nullstellensatz.common :as common]
            [nullstellensatz.object.combination :as combination]
            [schema.core :as s]))

(defn- enumerate*
  "Catalan number C_n = (1/(n+1)) * binom(2n, n)."
  [n]
  (let [binomial-val (combination/enumerate (*' 2 n) n)]
    (quot binomial-val (inc n))))

(def enumerate (memoize enumerate*))

(defn- count-dyck-paths*
  "Number of Dyck paths from (x,y) to (2n,0) via the reflection principle."
  [n x y]
  (if (> (+' x y) (*' 2 n)) 0
      (let [average (quot (+' x y) 2)
            dynamic-set (-' (*' 2 n) x)
            p (combination/enumerate dynamic-set (-' n average))
            q (combination/enumerate dynamic-set (-' n 1 average))]
        (-' p q))))

(def count-dyck-paths (memoize count-dyck-paths*))

(defn- unrank-step [n i x y low answer]
  (if (= x ((comp inc *') n 2)) answer
      (let [m (count-dyck-paths n x (inc y))
            jump? (<= i ((comp dec +') low m))]
        (recur n i
               (inc x)
               (if jump? (inc y) (dec y))
               (if jump? low (+' low m))
               (-> jump? (if 1 0) (cons answer))))))

(defn unrank [n m]
  ((comp vec unrank-step) n m 1 0 0 []))

(defn generate
  "It generates a Dyck path"
  [n m]
  (let [code (unrank n m)]
    (loop [x 1 y 0 answer [[0 0]]]
      (if (> x (*' 2 n)) answer
          (let [x-val (inc x)
                y-val (as-> x $
                        (dec $)
                        (get code $)
                        (zero? $)
                        (if $ (inc y) (dec y)))]
            (recur x-val y-val (conj answer [x y-val])))))))

(s/defschema EnumerateSchema
  {:n s/Int})

(def export-enumerate
  (common/->Export
   EnumerateSchema ::enumerate
   (fn [{:keys [n]}] (enumerate n))))

(s/defschema GenerateSchema
  {:n s/Int :m s/Int})

(def export-generate
  (common/->Export
   GenerateSchema ::generate
   (fn [{:keys [n m]}] (generate n m))))
