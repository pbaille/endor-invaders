(ns endor-invaders.core-test
  (:require [endor-invaders.core :as i]
            [clojure.test :as t :refer [deftest testing is]]
            [clojure.string :as str]))

(def invader1
  "--o-----o--
---o---o---
--ooooooo--
-oo-ooo-oo-
ooooooooooo
o-ooooooo-o
o-o-----o-o
---oo-oo---")

(def invader2
  "---oo---
--oooo--
-oooooo-
oo-oo-oo
oooooooo
--o--o--
-o-oo-o-
o-o--o-o")

(def radar-sample
  "----o--oo----o--ooo--ooo--o------o---oo-o----oo---o--o---------o----o------o-------------o--o--o--o-
--o-o-----oooooooo-oooooo---o---o----o------ooo-o---o--o----o------o--o---ooo-----o--oo-o------o----
--o--------oo-ooo-oo-oo-oo-----O------------ooooo-----oo----o------o---o--o--o-o-o------o----o-o-o--
-------o--oooooo--o-oo-o--o-o-----oo--o-o-oo--o-oo-oo-o--------o-----o------o-ooooo---o--o--o-------
------o---o-ooo-ooo----o-----oo-------o---oo-ooooo-o------o----o--------o-oo--ooo-oo-------------o-o
-o--o-----o-o---o-ooooo-o-------oo---o---------o-----o-oo-----------oo----ooooooo-ooo-oo------------
o-------------ooooo-o--o--o--o-------o--o-oo-oo-o-o-o----oo------------o--oooo--ooo-o----o-----o--o-
--o-------------------------oo---------oo-o-o--ooo----oo----o--o--o----o--o-o-----o-o------o-o------
-------------------o----------o------o--o------o--------o--------o--oo-o-----oo-oo---o--o---o-----oo
----------o----------o---o--------------o--o----o--o-o------------oo------o--o-o---o-----o----------
------o----o-o---o-----o-o---o-----oo-o--------o---------------------------------o-o-o--o-----------
---------------o-------o-----o-------o-------------------o-----o---------o-o-------------o-------oo-
-o--o-------------o-o-----o--o--o--oo-------------o----ooo----o-------------o----------oo----o---o-o
-o--o-------------o----oo------o--o-------o--o-----o-----o----o-----o--o----o--oo-----------o-------
-o-----oo-------o------o----o----------o--o----o-----o-----o-------o-----------o---o-o--oooooo-----o
-o--------o-----o-----o---------oo----oo---o-o---------o---o--oooo-oo--o-------o------oo--oo--o-----
------------o---------o---------o----oooo-------------oo-oo-----ooo-oo-----o-------o-oo-oooooooo---o
----------------------o------------oooooooo---o-----o-------o--oooooo-o------------o-o-ooooooo-o----
------------o------o---o---o-------oo-oo--o--o---------o--o-o-o-ooooo-o--------------oo-o----o-oo-o-
---o-o----------oo-------oo----o----oooooooo-------o----o-o-o-o-----o-o-----o----------ooo-oo--o---o
-o-o---------o-o---------------o--o--o--ooo---ooo-------o------oo-oo------------o--------o--o-o--o--
-------oo---------------------------o-oo----------o------o-o-------o-----o----o-----o-oo-o-----o---o
---o--------o-----o-------o-oo-----oo--oo-o----oo----------o--o---oo------oo----o-----o-------o-----
---o--ooo-o---------o-o----o------------o---------o----o--o-------o----o--------o----------------oo-
---o------o----------------o----o------o------o---oo-----------o-------------o----------oo---------o
--oo---------------o--o------o---o-----o--o-------------o------o-------o-----o-----o----o------o--o-
-o-------o----------o-o-o-------o-----o--o-o-----------o-oo-----------o------o---------o-----o-o----
----------o----o-------o----o--o------o------------o---o---------------oo----o-----ooo--------------
----o--------oo----o-o----o--o------ooo----o-oooo---o--o-oo--------o-oo-----o-o---o-o--o-----oo-----
------o--------o-ooooo----o---o--o-----o---------------o-o-------o-----o----------------------------
o-------oo----o--oooooo-o---o--o------oooo----------o-oo-------o---o----------o------oo-------------
-o---o----------o--oo-oo-o---o-----o-o-----------------------oo--o------o------o--------------------
-----oo-o-o-o---ooooooooo----o----o--------o--o---oo---o------------o----------o-o---o------o-o--oo-
------o------o---ooo-o---------------------------o--o---o---o----o--o-------o-----o------o----o----o
-------o----------ooo-o-----o----o---o--o-oo--o--o-o--o------o--o-oo---ooo------------------------o-
-o-------o------o-o--ooo--o---o---oo-----o----o-------------o----o-ooo-o------o--o-o------o-o-------
---oo--o---o-o---------o---o--------------o--o-----o-------o-----o--o---o-oo--------o----o----o-----
o------o----oo-o-----------oo--o---o--------o-o------o-------o-o------o-oo---------o-----oo---------
----o--o---o-o-----------o---o------------o-------o----o--o--o--o-o---------------o-----------------
-------oo--o-o-----o-----o----o-o--o----------------------o-------o------o----oo----ooo---------o---
o-----oo-------------------o--o-----o-----------o------o-------o----o-----------o----------------o--
--o---o-------o------------o--------------------o----o--o-------------oo---o---------oo--------o----
--o--------o---------o------------o------o-------o------------o-------o---o---------ooooo-----------
------o--------------o-o-o---------o---o-------o--o-----o-------o-o----------o-----oo-ooo----------o
--o---------------o----o--oo-------------o---------o-------------------oo---------oo-o-ooo----------
-o-----------o------ooo----o----------------ooo-----o--------o--o---o-----------o-o-oooooo--------oo
-o---o-------o---o-oooo-----o-------------------o----oo-----------------o--o--------o--o------o--o--
-------o---o------oooooo--o----ooo--o--------o-------o----------------------------oo-oo-o--o--------
o--oo------o-----oo--o-oo------------oo--o------o--o-------------oo----o------------oooo-o------oo--
-----o----------ooooooooo--------------oo--------------oo-----o-----o-o--o------o----------o----o---")

(deftest endor-invaders-basics
  (testing "str->matrix"
    (is (= (i/str->matrix "---\n-o-\n---")
           [[0 0 0] [0 1 0] [0 0 0]]))
    (is (= (i/str->matrix "o-o\n-o-\no-o")
           [[1 0 1] [0 1 0] [1 0 1]]))
    (is (thrown? Exception
                 (i/str->matrix "o-\n-o-\no-o")))
    (is (thrown? Exception
                 (i/str->matrix "o-Z\n-o-\no-o"))))

  (testing "assert-matrix"
    (is (thrown? Exception
                 (i/assert-matrix [[1 0] [0 1 0] [1 0 1]])))
    (is (thrown? Exception
                 (i/assert-matrix [[1 0 3] [0 1 0] [1 0 1]])))
    (is (thrown? Exception
                 (i/assert-matrix :pouet))))

  (testing "matrix->str"
    (is (= (i/matrix->str [[0 0 0] [0 1 0] [0 0 0]])
           "---\n-o-\n---"))
    (is (= (i/matrix->str [[1 0 1] [0 1 0] [1 0 1]])
           "o-o\n-o-\no-o")))

  (testing "matrix-size"
    (is (= [2 2]
           (i/matrix-size [[0 1] [1 0]])))
    (is (= [2 3]
           (i/matrix-size [[0 1] [0 1] [1 0]]))))

  (testing "assignment data"
    (is (->> invader1
             i/str->matrix
             i/assert-matrix
             i/matrix->str
             (= invader1)))
    (is (->> invader2
             i/str->matrix
             i/assert-matrix
             i/matrix->str
             (= invader2)))
    (let [fixed-sample (str/replace radar-sample "O" "o")]
      (is (->> fixed-sample
               i/str->matrix
               i/assert-matrix
               i/matrix->str
               (= fixed-sample)))))

  (testing "matrix-similarity"
    (is (= 0 (i/matrix-similarity [[0] [1]] [[1] [0]])))
    (is (= 1 (i/matrix-similarity [[0 1] [1 0]] [[0 1] [1 0]])))
    (is (= 1/2 (i/matrix-similarity [[0 1] [1 0]] [[0 0] [0 0]])))
    (is (= 1/4
           (i/matrix-similarity [[0 1] [1 1]] [[0 0] [0 0]])
           (i/matrix-similarity [[1 0] [1 1]] [[0 0] [0 0]])))

    (is (= 1 (i/matrix-similarity (i/str->matrix invader1) (i/str->matrix invader1))))
    (is (= 1 (i/matrix-similarity (i/str->matrix invader2) (i/str->matrix invader2))))

    (is (thrown? Exception
                 (i/matrix-similarity [[0] [1]] [[0 1] [1 0]]))))

  (testing "sub-matrices"

    (is (= (i/sub-matrices [2 2]
                           [[0 1 1 0] [1 0 0 1] [1 1 1 1] [1 0 0 1]])
           (list {:content [[0 1] [1 0]], :position [0 0]}
                 {:content [[1 0] [1 1]], :position [0 1]}
                 {:content [[1 1] [1 0]], :position [0 2]}
                 {:content [[1 1] [0 0]], :position [1 0]}
                 {:content [[0 0] [1 1]], :position [1 1]}
                 {:content [[1 1] [0 0]], :position [1 2]}
                 {:content [[1 0] [0 1]], :position [2 0]}
                 {:content [[0 1] [1 1]], :position [2 1]}
                 {:content [[1 1] [0 1]], :position [2 2]})))

    (is (= (i/sub-matrices [3 2]
                           [[0 1 1 0] [1 0 0 1] [1 1 1 1] [1 0 0 1]])
           (list {:content [[0 1 1] [1 0 0]], :position [0 0]}
                 {:content [[1 0 0] [1 1 1]], :position [0 1]}
                 {:content [[1 1 1] [1 0 0]], :position [0 2]}
                 {:content [[1 1 0] [0 0 1]], :position [1 0]}
                 {:content [[0 0 1] [1 1 1]], :position [1 1]}
                 {:content [[1 1 1] [0 0 1]], :position [1 2]})))

    (is (= (i/sub-matrices [4 4]
                           [[0 1 1 0] [1 0 0 1] [1 1 1 1] [1 0 0 1]])
           (list {:content [[0 1 1 0] [1 0 0 1] [1 1 1 1] [1 0 0 1]], :position [0 0]})))
    (is (empty? (i/sub-matrices [5 2]
                                [[0 1 1 0] [1 0 0 1] [1 1 1 1] [1 0 0 1]]))))

  (testing "padding"
    (is (= (i/with-nil-padding [1 1] [[0 1] [1 0]])
           [[nil nil nil nil]
            [nil 0 1 nil]
            [nil 1 0 nil]
            [nil nil nil nil]]))
    (is (= (i/with-nil-padding [0 2] [[0 1] [1 0]])
           [[nil nil]
            [nil nil]
            [0 1]
            [1 0]
            [nil nil]
            [nil nil]]))
    (is (= (i/with-nil-padding [1 0] [[0 1] [1 0]])
           [[nil 0 1 nil] [nil 1 0 nil]]))))

(defn mk-matrix [x y f]
  (letfn [(mk-row []
            (vec (repeatedly x f))) ]
    (vec (repeatedly y mk-row))))

(defn rand-matrix [x y]
  (mk-matrix x y #(rand-nth [0 1])))

(defn matrix-elements [m]
  (->> (map (fn [y-idx row]
              (map (fn [x-idx v] [[y-idx x-idx] v])
                   (range)
                   row))
            (range)
            m)
       (reduce into [])))

(defn matrix-put
  [matrix [x y :as position] sub-matrix]
  (if-not (let [[sub-x-size sub-y-size] (i/matrix-size sub-matrix)
                [x-size y-size] (i/matrix-size matrix)]
            (and (>= x-size sub-x-size)
                 (>= y-size sub-y-size)))
    (throw (Exception. (str `matrix-put "not enought space.")))
    (reduce (fn [matrix [[y-offset x-offset] v]]
              (assoc-in matrix [(+ y y-offset) (+ x x-offset)] v))
            matrix
            (matrix-elements sub-matrix))))

(comment
  (matrix-put (rand-matrix 4 4)
              [1 1]
              [[9 9] [9 9]])

  (matrix-put (rand-matrix 6 7)
              [1 3]
              [[9 9] [9 9]])

  (matrix-put (rand-matrix 5 4)
              [1 1]
              [[9 9] [9 9]]))

(deftest endor-invaders-detection

  (testing "detect-matrix"

    (let [m (mk-matrix 10 10 (constantly 0))
          m1 [[1 1 1] [1 0 1] [1 1 1]]
          m2 [[1 0 1] [0 1 0] [1 0 1]]]

      (testing "trivial-detection"
        (is (= (i/detect-matrix {:radar-data m1
                                 :matrix m1
                                 :noise-tolerance 0.0
                                 :edge-overlap-ratio 0.0})
               [{:content m1
                 :similarity 1, :position [0 0]}]))

        (is (= (i/detect-matrix {:radar-data m2
                                 :matrix m2
                                 :noise-tolerance 0.0
                                 :edge-overlap-ratio 0.0})
               [{:content m2 :similarity 1, :position [0 0]}])))

      (testing "simple-detection"
        (let [m1-pos [3 5]
              m2-pos [1 6]]
          (is (= (i/detect-matrix {:radar-data (matrix-put m m1-pos m1)
                                   :matrix m1
                                   :noise-tolerance 0.0
                                   :edge-overlap-ratio 0.0})
                 [{:content m1 :similarity 1 :position m1-pos}]))

          (is (= (i/detect-matrix {:radar-data (matrix-put m m2-pos m2)
                                   :matrix m2
                                   :noise-tolerance 0.0
                                   :edge-overlap-ratio 0.0})
                 [{:content m2 :similarity 1 :position m2-pos}]))))

      (testing "edge-overlap-detection"
        (let [partial-m1 (vec (next m1))
              partial-m2 (mapv (comp vec butlast) m2)]
          (is (= (i/detect-matrix {:radar-data (matrix-put m [2 0] partial-m1)
                                   :matrix m1
                                   :noise-tolerance (/ 1.5 9)
                                   :edge-overlap-ratio 1/3})
                 [{:content [[nil nil nil]
                             [1 0 1]
                             [1 1 1]]
                   :similarity (/ 7.5 9)
                   :position [2 -1]}]))
          (is (= (i/detect-matrix {:radar-data (matrix-put m [8 3] partial-m2)
                                   :matrix m2
                                   :noise-tolerance (/ 1.5 9)
                                   :edge-overlap-ratio 1/3})
                 [{:content [[1 0 nil]
                             [0 1 nil]
                             [1 0 nil]]
                   :similarity (/ 7.5 9)
                   :position [8 3]}]))))

      (testing "noise-tolerance"
        (let [noisy-m1 [[1 1 0] [1 0 1] [1 1 1]]]
          (is (= (i/detect-matrix {:radar-data noisy-m1
                                   :matrix m1
                                   :noise-tolerance 1/9
                                   :edge-overlap-ratio 0.0})
                 [{:content noisy-m1
                   :similarity 8/9
                   :position [0 0]}]))

          (is (empty? (i/detect-matrix {:radar-data noisy-m1
                                        :matrix m1
                                        :noise-tolerance 1/10
                                        :edge-overlap-ratio 0.0}))))))

    (= (i/detect-matrix {:radar-data (i/str->matrix radar-sample)
                         :matrix (i/str->matrix invader1)
                         :edge-overlap-ratio 0.3
                         :noise-tolerance 0.2})
       [{:content [[0 0 1 0 0 0 0 0 1 0 0]
                   [0 0 0 0 0 0 0 1 0 0 0]
                   [0 0 1 1 1 1 0 1 1 0 0]
                   [0 0 0 0 1 1 1 0 1 1 0]
                   [1 0 0 1 1 1 1 1 1 0 1]
                   [1 0 1 0 1 1 1 1 1 0 1]
                   [1 0 1 0 0 0 0 0 1 0 1]
                   [0 0 0 1 1 0 1 1 0 0 0]],
         :similarity 10/11,
         :position [60 13]}
        {:content [[1 1 1 0 0 0 0 0 1 0 0]
                   [1 0 0 1 0 1 0 1 0 0 0]
                   [0 0 1 0 1 1 1 1 1 0 0]
                   [1 1 0 0 1 1 1 0 1 1 0]
                   [1 1 1 1 1 1 1 0 1 1 1]
                   [1 1 1 1 0 0 1 1 1 0 1]
                   [1 0 1 0 0 0 0 0 1 0 1]
                   [0 0 0 1 1 0 1 1 0 0 0]],
         :similarity 7/8,
         :position [74 1]}
        {:content [[0 0 1 1 0 0 0 0 1 0 0]
                   [0 0 0 0 0 0 0 1 0 0 0]
                   [1 0 0 1 1 1 1 1 1 0 0]
                   [0 1 1 0 0 1 1 0 0 1 0]
                   [1 1 0 1 1 1 1 1 1 1 1]
                   [1 0 1 1 1 1 1 1 1 0 1]
                   [1 1 0 1 0 0 0 0 1 0 1]
                   [0 0 1 1 1 0 1 1 0 0 1]],
         :similarity 19/22,
         :position [85 12]}])))
