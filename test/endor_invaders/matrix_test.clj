(ns endor-invaders.matrix-test
  (:require [endor-invaders.matrix :as matrix]
            [clojure.test :as t :refer [deftest testing is]]
            [clojure.string :as str]))

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
           [[nil 0 1 nil] [nil 1 0 nil]]))))
