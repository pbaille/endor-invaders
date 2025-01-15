(ns endor-invaders.core
  (:require [clojure.string :as str]
            [malli.core :as m]))

(do :validation

    (defn square-matrix-schema [x-size y-size]
      (into [:tuple]
            (repeat y-size
                    (into [:tuple] (repeat x-size [:enum 0 1])))))

    (comment
      (m/validate (square-matrix-schema 4 4)
                  [[0 1 1 0] [0 1 1 0] [0 1 1 0] [0 1 1 0]])
      (m/validate (square-matrix-schema 4 4)
                  [[1 1 0] [1 1 0] [1 1 0] [0 1 0]])))

(do :matrix

    (defn str->matrix [s]
      (let [lines (-> (str/trim s) (str/split #"\n"))]
        (if-not (apply = (map count lines))
          (throw (Exception. (str `str->matrix " invalid input string:\n\n" s)))
          (mapv (fn [line]
                  (mapv (fn [char]
                          (case char
                            \- 0
                            (\o \O) 1
                            (throw (Exception. (str `str->matrix " invalid character: " char)))))
                        line))
                lines))))

    (defn assert-matrix [m]
      (when-not (vector? m)
        (throw (Exception. (str `assert-matrix " not a vector:\n" m))))
      (when-not (every? (fn [row] (every? (partial contains? #{0 1 nil}) row)) m)
        (throw (Exception. (str 'assert-matrix " invalid content.\n" m))))
      (when-not (apply = (map count m))
        (throw (Exception. (str `assert-matrix " rows should be of equal length.\n" m))))
      m)

    (defn matrix->str [matrix]
      (->> matrix
           (map (fn [line]
                  (->> (map (fn [bit] (case bit 0 "-" 1 "o" nil "="))
                            line)
                       (apply str))))
           (str/join "\n")))

    (defn matrix-size [matrix]
      [(count (first matrix))
       (count matrix)])

    (defn matrix-similarity [matrix1 matrix2]

      (let [size (matrix-size matrix1)]

        (if-not (= size (matrix-size matrix2))

          (throw (Exception. (str `matrix-similarity " expects equal size matrices, got:"
                                  "\n\n"
                                  (matrix->str matrix1)
                                  "\n\n"
                                  (matrix->str matrix2))))

          (let [flat-results
                (mapcat (fn [row1 row2]
                          (map (fn [a b] (cond (or (nil? a) (nil? b)) nil
                                               (= a b) 1
                                               :else 0))
                               row1
                               row2))
                        matrix1
                        matrix2)

                {:keys [match-count unknown-count]}
                (reduce (fn [counts x]
                          (case x
                            1 (update counts :match-count inc)
                            nil (update counts :unknown-count inc)
                            0 counts))
                        {:match-count 0
                         :unknown-count 0}
                        flat-results)]

            (/ match-count
               (- (count flat-results)
                  unknown-count)))))))

(defn sub-matrices [[x-size y-size] matrix]
  (let [[x-total-size y-total-size] (matrix-size matrix)]
    (if (or (> x-size x-total-size)
            (> y-size y-total-size))
      []
      (let [x-partitions (mapv (fn [row]
                                 (vec (partition x-size 1 row)))
                               matrix)]
        (for [x-offset (range (inc (- x-total-size x-size)))
              y-offset (range (inc (- y-total-size y-size)))]
          (->> x-partitions
               (mapv (fn [partition] (vec (get partition x-offset))))
               (drop y-offset)
               (take y-size)
               (vec)
               (hash-map :position [x-offset y-offset]
                         :content)))))))

(defn with-nil-padding
  "Add `nil` padding to a the given `matrix`.
  `x-pad` and `y-pad` are respectively the numbers of columns and rows of the padding."
  [[x-pad y-pad] matrix]
  (let [[x-size _y-size] (matrix-size matrix)
        extra-rows (vec (repeat y-pad (vec (repeat (+ x-size (* 2 x-pad)) nil))))
        row-padding (repeat x-pad nil)]
    (vec (concat extra-rows
                 (map (fn [row]
                        (vec (concat row-padding row row-padding)))
                      matrix)
                 extra-rows))))

(defn detect-matrix
  [{:keys [radar-data matrix noise-tolerance edge-overlap-ratio]}]

  (let [[x-size y-size] (matrix-size matrix)
        ;; precise paddings depending on matrix size
        [x-pad y-pad] [(Math/round (float (* edge-overlap-ratio x-size)))
                       (Math/round (float (* edge-overlap-ratio y-size)))]
        ;; padded radar data
        radar-data (with-nil-padding [x-pad y-pad] radar-data)

        detections (->> (sub-matrices (matrix-size matrix) radar-data)
                        (mapv (fn [sub-matrix]
                                (assoc sub-matrix
                                       :similarity
                                       (matrix-similarity matrix (:content sub-matrix)))))
                        (sort-by :similarity >)
                        (take-while (fn [{:keys [similarity]}]
                                      (>= similarity (- 1 noise-tolerance)))))]
    ;; remove padding offset from detections positions
    (mapv (fn [x]
            (update x :position
                    (fn [[x y]] [(- x x-pad) (- y y-pad)])))
          detections)))

(defn detect [radar-sample
              {:keys [edge-overlap-ratio noise-tolerance shapes]}]

  (let [radar-data (str->matrix radar-sample)]

    (update-vals shapes
                 (fn [shape-str]
                   (detect-matrix {:edge-overlap-ratio (float edge-overlap-ratio)
                                   :noise-tolerance noise-tolerance
                                   :radar-data radar-data
                                   :matrix (str->matrix shape-str)})))))

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

(comment
  (comment
    (matrix->str (str->matrix invader1)))
  (comment
    (matrix-similarity
     (str->matrix invader1)
     (str->matrix invader1))

    (matrix-similarity
     (str->matrix invader1)
     (str->matrix invader2)))
  (comment
    (->> (sub-matrices (matrix-size (str->matrix invader1))
                       (str->matrix radar-sample))
         (mapv (fn [sub-matrix]
                 (assoc sub-matrix :similarity (float (matrix-similarity (str->matrix invader1)
                                                                         (:content sub-matrix))))))
         (sort-by :similarity >)
         (take 5)
         (map (fn [{:keys [position content]}]
                (println "\n\n------\nat: " position "\n\n")
                (println (matrix->str content)))))

    (mapv (fn [s]
            (println)
            (println s))
          (mapv matrix->str
                (sub-matrices [5 5]
                              (str->matrix radar-sample)))))

  (str->matrix invader1))
