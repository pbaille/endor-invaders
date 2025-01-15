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
