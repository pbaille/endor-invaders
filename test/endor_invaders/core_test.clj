(ns endor-invaders.core-test
  (:require [endor-invaders.core :as core]
            [endor-invaders.data :as data]
            [endor-invaders.matrix :as matrix]
            [clojure.test :as t :refer [deftest testing is]]
            [clojure.string :as str]
            [endor-invaders.test-utils :as tu]))

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

    (let [invader (core/str->matrix data/invader1)
          noisy-invader (update-in invader [3 4] {0 1 1 0})
          noisy-invader2 (-> invader
                             (update-in [2 5] {0 1 1 0})
                             (update-in [6 3] {0 1 1 0}))
          edge-overlapping-invader (vec (drop 2 invader))
          radar-sample (-> (tu/mk-matrix 100 100 (constantly 0))
                           (tu/matrix-put [34 35] invader)
                           (tu/matrix-put [0 0] noisy-invader)
                           (tu/matrix-put [23 9] noisy-invader2)
                           (tu/matrix-put [60 0] edge-overlapping-invader)
                           (core/matrix->str))]
      (is (= (core/detect radar-sample
                          {:shapes {:invader1 data/invader1}
                           :similarity-threshold 8/10})
             {:invader1 [{:content invader :similarity 1 :position [34 35]}
                         {:content noisy-invader :similarity 87/88 :position [0 0]}
                         {:content noisy-invader2 :similarity 86/88 :position [23 9]}
                         {:content (vec (concat (repeat 2 (vec (repeat 11 nil)))
                                                edge-overlapping-invader))
                          :similarity 0.875 :position [60 -2]}]})))

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
