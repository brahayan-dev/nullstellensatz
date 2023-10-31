(ns nullstellensatz.irreducible-linked-diagram)

(defrecord Pack [k-value
                 total-value
                 first-s-value
                 second-s-value])

(defn- ->pack [->value k-value n]
  (let [first-s-value (->value k-value)
        second-s-value (->value (- n k-value))
        total-value (* (- (* 2 k-value) 1) first-s-value second-s-value)]
    (Pack. k-value total-value first-s-value second-s-value)))

(defn- generate-packs [n]
  (letfn [(go [x]
              (if (<= x 2)
                [(Pack. 1 1 1 1)]
                (let [elements (range 1 x)
                      ->value #(->> (go %) (map :total-value) (apply +'))]
                  (map #(->pack ->value % x) elements))))]
    ((memoize go) n)))

(defn- select-pack [packs m]
  (let [t (rest packs)
        h (first packs)]
    (if (< m (:total-value h))
      [m h]
      (recur t (- m (:total-value h))))))

(defn- find-positions [[x first-s-value second-s-value]]
  (let [j (quot x (* first-s-value second-s-value))
        r (rem x (* first-s-value second-s-value))
        a (quot r second-s-value)
        b (rem r second-s-value)]
    [j a b]))

;; FIXME: What happen when k > 2|x| - 2?
(defn ->add [[k_ x y]]
  (let [k (inc k_)
        size-x (* 2 (count x))
        size-y (dec (* 2 (count y)))
        full-size (+ size-x size-y 1)
        ->first-part (fn [[a b]]
                       (cond
                         (and (> a k) (> b k)) [(+ a size-y) (+ b size-y)]
                         (> a k) [(+ a size-y) b]
                         (> b k) [a (+ b size-y)]
                         :else [a b]))
        ->second-part (fn [[a b]]
                        (cond
                          (= (+ b size-x) full-size) [(+ a k) (+ b size-x)]
                          :else [(+ a k) (+ b k)]))]
    (concat (map ->first-part x) (map ->second-part y))))

(defn ->code [n m]
  (cond
    (= n 1) [0 [0 0] [0 0]] ;; Convention: (1, 0)
    (= n 2) [0 [1 0] [1 0]]
    :else (let [packs (generate-packs n)
                [x pack] (select-pack packs m)
                [j a b] (find-positions
                         [x
                          (:first-s-value pack)
                          (:second-s-value pack)])
                k (:k-value pack)]
            [j [k a] [(- n k) b]])))

(defn ->structure [[j [k a] [p b]]]
  (if (= 0 (+ j k a p b))
    [[1 2]]
    (->add [j
            (->structure (->code k a))
            (->structure (->code p b))])))

(defn ->size [n]
  (let [packs (generate-packs n)]
    (->> packs (map :total-value) (apply +'))))
