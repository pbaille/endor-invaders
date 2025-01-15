(ns endor-invaders.test-utils-test
  (:require  [endor-invaders.matrix :as matrix]
             [endor-invaders.test-utils :as tu]
             [clojure.test :as t :refer [deftest testing is]]))

(deftest matrices
  (testing "mk-matrix"
    (is (= (tu/mk-matrix 0 0 (constantly 2))
           []))
    (is (= (tu/mk-matrix 0 2 (constantly 1))
           [[] []]))
    (is (= (tu/mk-matrix 3 2 (constantly 0))
           [[0 0 0] [0 0 0]]))
    (is (= (tu/mk-matrix 2 3 (constantly :io))
           [[:io :io] [:io :io] [:io :io]])))

  (testing "rand-bit-matrix"
    (let [m (tu/rand-bit-matrix 3 3)]
      (is (= [3 3] (matrix/size m)))
      (is (every? #{0 1} (mapcat identity m)))))

  (testing "matrix-put"
    []
    (let [m (tu/mk-matrix 4 4 (constantly 0))]
      (is (= (tu/matrix-put m [0 0] [[9 9] [9 9]])
             [[9 9 0 0] [9 9 0 0] [0 0 0 0] [0 0 0 0]]))
      (is (= (tu/matrix-put m [1 1] [[9 9] [9 9]])
             [[0 0 0 0] [0 9 9 0] [0 9 9 0] [0 0 0 0]]))
      (is (thrown? Exception
                   (tu/matrix-put m [1 3] [[9 9] [9 9]]))))))
