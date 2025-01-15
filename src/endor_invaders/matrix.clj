(ns endor-invaders.matrix)

(defn size [matrix]
  [(count (first matrix))
   (count matrix)])

(defn similarity [matrix1 matrix2]

  (let [size1 (size matrix1)]

    (if-not (= size1 (size matrix2))

      (throw (Exception. (str `matrix-similarity " expects equal size matrices, got:"
                              (size matrix1) " and " (size matrix2))))

      (let [flat-results
            (mapcat (fn [row1 row2]
                      (map (fn [a b] (cond (or (nil? a) (nil? b)) 0.5
                                           (= a b) 1
                                           :else 0))
                           row1
                           row2))
                    matrix1
                    matrix2)]

        (/ (reduce + 0 flat-results)
           (count flat-results))))))

(defn sub-matrices [[x-size y-size] matrix]
  (let [[x-total-size y-total-size] (size matrix)]
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

(defn pad
  "Add padding elements to a the given `matrix`.
  `x-pad` and `y-pad` are respectively the numbers of columns and rows of the padding.
   `elem` is the value used in padding cells"
  [elem [x-pad y-pad] matrix]
  (let [[x-size _y-size] (size matrix)
        extra-rows (vec (repeat y-pad (vec (repeat (+ x-size (* 2 x-pad)) elem))))
        row-padding (repeat x-pad elem)]
    (vec (concat extra-rows
                 (map (fn [row]
                        (vec (concat row-padding row row-padding)))
                      matrix)
                 extra-rows))))

(defn cells [m]
  (->> (map (fn [y-idx row]
              (map (fn [x-idx v] [[y-idx x-idx] v])
                   (range)
                   row))
            (range)
            m)
       (reduce into [])))
