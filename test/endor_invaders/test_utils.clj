(ns endor-invaders.test-utils
  (:require [endor-invaders.matrix :as matrix]))

(defn mk-matrix [x y f]
  (letfn [(mk-row []
            (vec (repeatedly x f)))]
    (vec (repeatedly y mk-row))))

(defn rand-bit-matrix [x y]
  (mk-matrix x y #(rand-nth [0 1])))

(defn matrix-put
  [matrix [x y] sub-matrix]
  (if-not (let [[sub-x-size sub-y-size] (matrix/size sub-matrix)
                [x-size y-size] (matrix/size matrix)]
            (and (>= x-size (+ x sub-x-size))
                 (>= y-size (+ y sub-y-size))))
    (throw (Exception. (str `matrix-put "not enought space.")))
    (reduce (fn [matrix [[y-offset x-offset] v]]
              (assoc-in matrix [(+ y y-offset) (+ x x-offset)] v))
            matrix
            (matrix/cells sub-matrix))))
