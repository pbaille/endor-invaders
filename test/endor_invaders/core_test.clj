(ns endor-invaders.core-test
  (:require [endor-invaders.core :as core]
            [endor-invaders.data :as data]
            [endor-invaders.matrix :as matrix]
            [clojure.test :as t :refer [deftest testing is]]
            [clojure.string :as str]))

(deftest conversions
  (testing "str->matrix"
    (is (= (core/str->matrix "---\n-o-\n---")
           [[0 0 0] [0 1 0] [0 0 0]]))
    (is (= (core/str->matrix "o-o\n-o-\no-o")
           [[1 0 1] [0 1 0] [1 0 1]]))
    (is (thrown? Exception
                 (core/str->matrix "o-\n-o-\no-o")))
    (is (thrown? Exception
                 (core/str->matrix "o-Z\n-o-\no-o"))))

  (testing "assert-matrix"
    (is (thrown? Exception
                 (core/assert-matrix [[1 0] [0 1 0] [1 0 1]])))
    (is (thrown? Exception
                 (core/assert-matrix [[1 0 3] [0 1 0] [1 0 1]])))
    (is (thrown? Exception
                 (core/assert-matrix :pouet))))

  (testing "matrix->str"
    (is (= (core/matrix->str [[0 0 0] [0 1 0] [0 0 0]])
           "---\n-o-\n---"))
    (is (= (core/matrix->str [[1 0 1] [0 1 0] [1 0 1]])
           "o-o\n-o-\no-o")))

  (testing "assignment data"
    (is (->> data/invader1
             core/str->matrix
             core/assert-matrix
             core/matrix->str
             (= data/invader1)))
    (is (->> data/invader2
             core/str->matrix
             core/assert-matrix
             core/matrix->str
             (= data/invader2)))
    (let [fixed-sample (str/replace data/radar-sample "O" "o")]
      (is (->> fixed-sample
               core/str->matrix
               core/assert-matrix
               core/matrix->str
               (= fixed-sample))))))

(deftest detections

  (testing "on data"
    (is (= (matrix/detect-sub-matrix {:matrix (core/str->matrix data/radar-sample)
                                      :sub-matrix (core/str->matrix data/invader1)
                                      :similarity-threshold 0.8})
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
             :position [85 12]}]))

    (is (= (core/detect data/radar-sample
                        {:shapes {:invader1 data/invader1
                                  :invader2 data/invader2}
                         :similarity-threshold 0.8})
           {:invader1 [{:content [[0 0 1 0 0 0 0 0 1 0 0]
                                  [0 0 0 0 0 0 0 1 0 0 0]
                                  [0 0 1 1 1 1 0 1 1 0 0]
                                  [0 0 0 0 1 1 1 0 1 1 0]
                                  [1 0 0 1 1 1 1 1 1 0 1]
                                  [1 0 1 0 1 1 1 1 1 0 1]
                                  [1 0 1 0 0 0 0 0 1 0 1]
                                  [0 0 0 1 1 0 1 1 0 0 0]],
                        :similarity 10/11, :position [60 13]}
                       {:content [[1 1 1 0 0 0 0 0 1 0 0]
                                  [1 0 0 1 0 1 0 1 0 0 0]
                                  [0 0 1 0 1 1 1 1 1 0 0]
                                  [1 1 0 0 1 1 1 0 1 1 0]
                                  [1 1 1 1 1 1 1 0 1 1 1]
                                  [1 1 1 1 0 0 1 1 1 0 1]
                                  [1 0 1 0 0 0 0 0 1 0 1]
                                  [0 0 0 1 1 0 1 1 0 0 0]],
                        :similarity 7/8, :position [74 1]}
                       {:content [[0 0 1 1 0 0 0 0 1 0 0]
                                  [0 0 0 0 0 0 0 1 0 0 0]
                                  [1 0 0 1 1 1 1 1 1 0 0]
                                  [0 1 1 0 0 1 1 0 0 1 0]
                                  [1 1 0 1 1 1 1 1 1 1 1]
                                  [1 0 1 1 1 1 1 1 1 0 1]
                                  [1 1 0 1 0 0 0 0 1 0 1]
                                  [0 0 1 1 1 0 1 1 0 0 1]],
                        :similarity 19/22, :position [85 12]}],
            :invader2 [{:content [[0 0 0 1 1 0 0 0]
                                  [0 0 1 1 1 0 1 0]
                                  [0 0 1 1 1 1 1 0]
                                  [1 1 0 0 1 0 1 1]
                                  [1 1 0 1 1 1 1 1]
                                  [0 0 0 0 0 1 0 0]
                                  [1 1 0 1 1 0 1 0]
                                  [1 0 1 0 0 1 1 1]],
                        :similarity 7/8, :position [42 0]}
                       {:content [[0 0 0 1 0 1 0 0]
                                  [0 1 1 1 1 1 0 0]
                                  [0 1 1 1 1 1 1 0]
                                  [1 0 0 1 1 0 1 1]
                                  [1 1 1 1 1 1 1 1]
                                  [0 1 1 1 0 1 0 0]
                                  [0 0 1 1 1 0 1 0]
                                  [1 0 1 0 0 1 1 1]],
                        :similarity 55/64, :position [16 28]}
                       {:content [[0 0 0 1 1 0 0 0]
                                  [0 0 1 1 1 1 1 0]
                                  [0 1 1 0 1 1 1 0]
                                  [1 1 0 1 0 1 1 1]
                                  [1 0 1 1 1 1 1 1]
                                  [0 0 1 0 0 1 0 0]
                                  [1 1 0 1 1 0 1 0]
                                  [0 0 1 1 1 1 0 1]],
                        :similarity 55/64, :position [82 41]}
                       {:content [[0 0 0 1 1 0 0 0]
                                  [0 0 1 1 1 1 0 0]
                                  [1 1 1 1 1 1 1 1]
                                  [1 1 0 1 1 0 0 1]
                                  [0 1 1 1 1 1 1 1]
                                  [0 0 1 0 0 1 1 1]
                                  [0 1 0 1 1 0 0 0]
                                  [1 1 0 0 1 1 0 1]],
                        :similarity 27/32, :position [35 15]}]}))))
