(ns endor-invaders.core
  (:require [clojure.string :as str]
            [endor-invaders.matrix :as matrix]))

;; conversions ---------------------------------

(def CHAR->VAL
  {\- 0
   \o 1
   \O 1})

(def VAL->CHAR
  {0 \-
   1 \o
   nil \=})

(defn str->matrix
  "Turn the string `s` to a matrix.
   `s` should a multiline string of `-` and `o` characters, each line should be of equal length."
  [s]
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

;; detection ------------------------------------------

(defn detect
  "Detect `shapes` in a `radar-sample` accordingly to `similarity-threshold`.
   `radar-sample` is a multiline string of `-` and `o` characters, each line should be of equal length.
   `shapes` is a map of keyword to string (same type of string than radar-sample)
   `similarity-threshold` is a number between 0 and 1, sensibility of the detection (1 for exact matches only, 0 for paranoid mode)"
  [radar-sample
   {:keys [similarity-threshold shapes]}]

  (let [radar-data (str->matrix radar-sample)]

    (update-vals shapes
                 (fn [shape-str]
                   (matrix/detect-sub-matrix {:matrix radar-data
                                              :sub-matrix (str->matrix shape-str)
                                              :similarity-threshold similarity-threshold})))))
