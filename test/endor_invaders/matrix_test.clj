(ns endor-invaders.matrix-test
  (:require [endor-invaders.matrix :as matrix]
            [clojure.test :as t :refer [deftest testing is]]
            [endor-invaders.test-utils :as tu]))

(deftest endor-invaders-basics
  (testing "matrix-size"
    (is (= [2 2]
           (matrix/size [[0 1] [1 0]])))
    (is (= [2 3]
           (matrix/size [[0 1] [0 1] [1 0]]))))

  (testing "matrix-similarity"
    (is (= 0 (matrix/similarity [[0] [1]] [[1] [0]])))
    (is (= 1 (matrix/similarity [[0 1] [1 0]] [[0 1] [1 0]])))
    (is (= 1/2 (matrix/similarity [[0 1] [1 0]] [[0 0] [0 0]])))
    (is (= 1/4
           (matrix/similarity [[0 1] [1 1]] [[0 0] [0 0]])
           (matrix/similarity [[1 0] [1 1]] [[0 0] [0 0]])))

    (is (thrown? Exception
                 (matrix/similarity [[0] [1]] [[0 1] [1 0]]))))

  (testing "sub-matrices"

    (is (= (matrix/sub-matrices [2 2]
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

    (is (= (matrix/sub-matrices [3 2]
                                [[0 1 1 0] [1 0 0 1] [1 1 1 1] [1 0 0 1]])
           (list {:content [[0 1 1] [1 0 0]], :position [0 0]}
                 {:content [[1 0 0] [1 1 1]], :position [0 1]}
                 {:content [[1 1 1] [1 0 0]], :position [0 2]}
                 {:content [[1 1 0] [0 0 1]], :position [1 0]}
                 {:content [[0 0 1] [1 1 1]], :position [1 1]}
                 {:content [[1 1 1] [0 0 1]], :position [1 2]})))

    (is (= (matrix/sub-matrices [4 4]
                                [[0 1 1 0] [1 0 0 1] [1 1 1 1] [1 0 0 1]])
           (list {:content [[0 1 1 0] [1 0 0 1] [1 1 1 1] [1 0 0 1]], :position [0 0]})))
    (is (empty? (matrix/sub-matrices [5 2]
                                     [[0 1 1 0] [1 0 0 1] [1 1 1 1] [1 0 0 1]]))))

  (testing "padding"
    (is (= (matrix/pad nil [1 1] [[0 1] [1 0]])
           [[nil nil nil nil]
            [nil 0 1 nil]
            [nil 1 0 nil]
            [nil nil nil nil]]))
    (is (= (matrix/pad nil [0 2] [[0 1] [1 0]])
           [[nil nil]
            [nil nil]
            [0 1]
            [1 0]
            [nil nil]
            [nil nil]]))
    (is (= (matrix/pad nil [1 0] [[0 1] [1 0]])
           [[nil 0 1 nil] [nil 1 0 nil]])))

  (testing "detect-sub-matrix"

    (let [m (tu/mk-matrix 10 10 (constantly 0))
          m1 [[1 1 1] [1 0 1] [1 1 1]]
          m2 [[1 0 1] [0 1 0] [1 0 1]]]

      (testing "trivial-detection"
        (is (= (matrix/detect-sub-matrix {:matrix m1
                                          :sub-matrix m1
                                          :similarity-threshold 1})
               [{:content m1
                 :similarity 1, :position [0 0]}]))

        (is (= (matrix/detect-sub-matrix {:matrix m2
                                          :sub-matrix m2
                                          :similarity-threshold 1})
               [{:content m2 :similarity 1, :position [0 0]}])))

      (testing "simple-detection"
        (let [m1-pos [3 5]
              m2-pos [1 6]]
          (is (= (matrix/detect-sub-matrix {:matrix (tu/matrix-put m m1-pos m1)
                                            :sub-matrix m1
                                            :similarity-threshold 1})
                 [{:content m1 :similarity 1 :position m1-pos}]))

          (is (= (matrix/detect-sub-matrix {:matrix (tu/matrix-put m m2-pos m2)
                                            :sub-matrix m2
                                            :similarity-threshold 1})
                 [{:content m2 :similarity 1 :position m2-pos}]))))

      (testing "edge-overlap-detection"
        (let [partial-m1 (vec (next m1))
              partial-m2 (mapv (comp vec butlast) m2)]
          (is (= (matrix/detect-sub-matrix {:matrix (tu/matrix-put m [2 0] partial-m1)
                                            :sub-matrix m1
                                            :similarity-threshold (/ 7 9)})
                 [{:content [[nil nil nil]
                             [1 0 1]
                             [1 1 1]]
                   :similarity (/ 7.5 9)
                   :position [2 -1]}]))
          (is (= (matrix/detect-sub-matrix {:matrix (tu/matrix-put m [8 3] partial-m2)
                                            :sub-matrix m2
                                            :similarity-threshold (/ 7 9)})
                 [{:content [[1 0 nil]
                             [0 1 nil]
                             [1 0 nil]]
                   :similarity (/ 7.5 9)
                   :position [8 3]}]))))

      (testing "noise-tolerance"
        (let [noisy-m1 [[1 1 0] [1 0 1] [1 1 1]]]
          (is (= (matrix/detect-sub-matrix {:matrix noisy-m1
                                            :sub-matrix m1
                                            :similarity-threshold 8/9})
                 [{:content noisy-m1
                   :similarity 8/9
                   :position [0 0]}]))

          (is (empty? (matrix/detect-sub-matrix {:matrix noisy-m1
                                                 :sub-matrix m1
                                                 :similarity-threshold 9/10}))))))))
