(ns endor-invaders.core
  (:require [clojure.string :as str]
            [endor-invaders.matrix :as matrix]))

;; string to matrix conversions ---------------------------------

(def CHAR->VAL
  {\- 0
   \o 1
   \O 1})

(def VAL->CHAR
  {0 \-
   1 \o
   nil \=})

(defn str->matrix [s]
  (let [lines (-> (str/trim s) (str/split #"\n"))]
    (if-not (apply = (map count lines))
      (throw (Exception. (str `str->matrix " invalid input string:\n\n" s)))
      (mapv (fn [line]
              (mapv (fn [char]
                      (or (CHAR->VAL char)
                          (throw (Exception. (str `str->matrix " invalid character: " char)))))
                    line))
            lines))))

(defn assert-matrix [m]
  (when-not (vector? m)
    (throw (Exception. (str `assert-matrix " not a vector:\n" m))))
  (when-not (every? (fn [row] (every? VAL->CHAR row)) m)
    (throw (Exception. (str 'assert-matrix " invalid content.\n" m))))
  (when-not (apply = (map count m))
    (throw (Exception. (str `assert-matrix " rows should be of equal length.\n" m))))
  m)

(defn matrix->str [matrix]
  (->> matrix
       (map (fn [line]
              (->> (map (fn [val]
                          (or (VAL->CHAR val)
                              (throw (Exception. (str `matrix->str " invalid value: " val)))))
                        line)
                   (apply str))))
       (str/join "\n")))

;; sub-matrix detections ------------------------------------------

(defn detect-sub-matrix
  [{:keys [matrix sub-matrix similarity-treshold]}]

  (let [uncertainty (- 1.0 similarity-treshold)

        [x-size y-size :as sub-matrix-size] (matrix/size sub-matrix)
        ;; precise paddings depending on sub-matrix size
        [x-pad y-pad] [(Math/round (* 2 uncertainty x-size))
                       (Math/round (* 2 uncertainty y-size))]
        ;; padded matrix
        matrix (matrix/pad nil [x-pad y-pad] matrix)

        detections (->> (matrix/sub-matrices sub-matrix-size matrix)
                        (mapv (fn [sm]
                                (assoc sm
                                       :similarity
                                       (matrix/similarity sub-matrix (:content sm)))))
                        (sort-by :similarity >)
                        (take-while (fn [{:keys [similarity]}]
                                      (>= similarity similarity-treshold))))]
    ;; remove padding offset from detections positions
    (mapv (fn [x]
            (update x :position
                    (fn [[x y]] [(- x x-pad) (- y y-pad)])))
          detections)))


(defn detect [radar-sample
              {:keys [similarity-treshold shapes]}]

  (let [radar-data (str->matrix radar-sample)]

    (update-vals shapes
                 (fn [shape-str]
                   (detect-sub-matrix {:matrix radar-data
                                       :sub-matrix (str->matrix shape-str)
                                       :similarity-treshold similarity-treshold})))))

(defn print-detections [detections]
  (mapv (fn [[type results]]
          (println "DETECTED: " type)
          (mapv (fn [{:keys [position content similarity]}]
                  (println
                   (str "\nat: " position
                        "\nprob: " (format "%.2f" (float similarity))
                        "\n::\n"
                        (matrix->str content)
                        "\n")))
                results)
          (println "------------------------"))
        detections))

(do :data

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
-----o----------ooooooooo--------------oo--------------oo-----o-----o-o--o------o----------o----o---"))

(comment
  (-> (detect radar-sample
              {:shapes {:invader1 invader1
                        :invader2 invader2}
               :noise-tolerance 1/4
               :edge-overlap-ratio 0.3})
      print-detections))
