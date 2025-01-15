(ns endor-invaders.core-test
  (:require [endor-invaders.core :as core]
            [endor-invaders.data :as data]
            [endor-invaders.test-utils :as tu]
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

  (testing "detect-sub-matrix"

    (let [m (tu/mk-matrix 10 10 (constantly 0))
          m1 [[1 1 1] [1 0 1] [1 1 1]]
          m2 [[1 0 1] [0 1 0] [1 0 1]]]

      (testing "trivial-detection"
        (is (= (core/detect-sub-matrix {:matrix m1
                                        :sub-matrix m1
                                        :similarity-treshold 1})
               [{:content m1
                 :similarity 1, :position [0 0]}]))

        (is (= (core/detect-sub-matrix {:matrix m2
                                        :sub-matrix m2
                                        :similarity-treshold 1})
               [{:content m2 :similarity 1, :position [0 0]}])))

      (testing "simple-detection"
        (let [m1-pos [3 5]
              m2-pos [1 6]]
          (is (= (core/detect-sub-matrix {:matrix (tu/matrix-put m m1-pos m1)
                                          :sub-matrix m1
                                          :similarity-treshold 1})
                 [{:content m1 :similarity 1 :position m1-pos}]))

          (is (= (core/detect-sub-matrix {:matrix (tu/matrix-put m m2-pos m2)
                                          :sub-matrix m2
                                          :similarity-treshold 1})
                 [{:content m2 :similarity 1 :position m2-pos}]))))

      (testing "edge-overlap-detection"
        (let [partial-m1 (vec (next m1))
              partial-m2 (mapv (comp vec butlast) m2)]
          (is (= (core/detect-sub-matrix {:matrix (tu/matrix-put m [2 0] partial-m1)
                                          :sub-matrix m1
                                          :similarity-treshold (/ 7 9)})
                 [{:content [[nil nil nil]
                             [1 0 1]
                             [1 1 1]]
                   :similarity (/ 7.5 9)
                   :position [2 -1]}]))
          (is (= (core/detect-sub-matrix {:matrix (tu/matrix-put m [8 3] partial-m2)
                                          :sub-matrix m2
                                          :similarity-treshold (/ 7 9)})
                 [{:content [[1 0 nil]
                             [0 1 nil]
                             [1 0 nil]]
                   :similarity (/ 7.5 9)
                   :position [8 3]}]))))

      (testing "noise-tolerance"
        (let [noisy-m1 [[1 1 0] [1 0 1] [1 1 1]]]
          (is (= (core/detect-sub-matrix {:matrix noisy-m1
                                          :sub-matrix m1
                                          :similarity-treshold 8/9})
                 [{:content noisy-m1
                   :similarity 8/9
                   :position [0 0]}]))

          (is (empty? (core/detect-sub-matrix {:matrix noisy-m1
                                               :sub-matrix m1
                                               :similarity-treshold 9/10}))))))

    (testing "on data"
      (is (= (core/detect-sub-matrix {:matrix (core/str->matrix data/radar-sample)
                                      :sub-matrix (core/str->matrix data/invader1)
                                      :similarity-treshold 0.8})
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
                           :similarity-treshold 0.8})
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
                          :similarity 27/32, :position [35 15]}]})))))
