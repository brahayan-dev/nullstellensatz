(ns nullstellensatz.object.catalan-family
  (:require [nullstellensatz.object.combination :as combination]))

(defn enumerate [n]
  (let [division-val (/ 1 (inc n))
        binomial-val (combination/enumerate (*' 2 n) n)]
    (*' division-val binomial-val)))

(defn count-mountain-ranges [n x y]
  (if (> (+' x y) (*' 2 n)) 0
      (let [average (quot (+' x y) 2)
            dynamic-set (-' (*' 2 n) x)
            p (combination/enumerate dynamic-set (-' n average))
            q (combination/enumerate dynamic-set (-' n 1 average))]
        (-' p q))))

(defn- unrank-helper [n i x y low answer]
  (if (= x ((comp inc *') n 2)) answer
      (let [m (count-mountain-ranges n x (inc y))
            jump? (<= i ((comp dec +') low m))]
        (recur n i
               (inc x)
               (if jump? (inc y) (dec y))
               (if jump? low (+' low m))
               (-> jump? (if 1 0) (cons answer))))))

(defn unrank [n i]
  ((comp vec unrank-helper) n i 1 0 0 []))

(defn generate
  "It generates a Dyck path"
  [n r]
  (let [code (unrank n r)]
    (loop [x 1 y 0 answer [[0 0]]]
      (if (> x (*' 2 n)) answer
          (let [x-val (inc x)
                y-val (as-> x $
                        (dec $)
                        (get code $)
                        (zero? $)
                        (if $ (inc y) (dec y)))]
            (recur x-val y-val (conj answer [x y-val])))))))
