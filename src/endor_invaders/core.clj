(ns endor-invaders.core
  (:require [clojure.string :as str]
            [malli.core :as m]))

(do :validation

    (defn square-shape-schema [x-size y-size]
      (into [:tuple]
            (repeat y-size
                    (into [:tuple] (repeat x-size [:enum 0 1])))))

    (comment
      (m/validate (square-shape-schema 4 4)
                  [[0 1 1 0] [0 1 1 0] [0 1 1 0] [0 1 1 0]])
      (m/validate (square-shape-schema 4 4)
                  [[1 1 0] [1 1 0] [1 1 0] [0 1 0]])))

(do :shape

    (defn str->shape [s]
      (mapv (fn [line]
              (mapv (fn [char] (case char \- 0 (\o \O) 1))
                    line))
            (str/split s #"\n")))

    (defn shape->str [shape]
      (->> shape
           (map (fn [line]
                  (->> (map (fn [bit] (case bit 0 "-" 1 "o" nil "/"))
                            line)
                       (apply str))))
           (str/join "\n")))

    (defn shape? [v]
      (and (apply = (map count v))
           (every? #{0 1} (flatten v))))

    (defn shape-size [v]
      [(count (first v))
       (count v)])

    (defn shape-similarity [shape1 shape2]
      (if (= (shape-size shape1)
             (shape-size shape2))
        (let [score (->> (mapv (fn [line1 line2]
                                 (->> (mapv (fn [a b] (if (= a b) 1 0))
                                            line1
                                            line2)
                                      (reduce +)))
                               shape1
                               shape2)
                         (reduce +))
              element-count (apply * (shape-size shape1))]
          (/ score
             element-count))
        (throw (Exception. (str `shape-similarity " expects equal size shapes, got:"
                                "\n\n"
                                (shape->str shape1)
                                "\n\n"
                                (shape->str shape2)))))))

(defn sub-shapes [[x-size y-size] shape]
  (let [[x-total-size y-total-size] (shape-size shape)]
    (if (or (> x-size x-total-size)
            (> y-size y-total-size))
      []
      (let [x-partitions (mapv (fn [line]
                                 (vec (partition x-size 1 line)))
                               shape)]
        (for [x-offset (range (- x-total-size x-size))
              y-offset (range (- y-total-size y-size))]
          (->> x-partitions
               (mapv (fn [partition] (vec (get partition x-offset))))
               (drop y-offset)
               (take y-size)
               (vec)
               (hash-map :position [x-offset y-offset]
                         :content)))))))

(defn detect-shape [{:keys [radar-data shape noise-tolerance]}]
  (->> (sub-shapes (shape-size shape) radar-data)
       (mapv (fn [sub-shape]
               (assoc sub-shape
                      :similarity
                      (shape-similarity shape (:content sub-shape)))))
       (sort-by :similarity >)
       (take-while (fn [{:keys [similarity]}]
                     (> similarity (- 1 noise-tolerance))))))

(defn detect [radar-sample
              {:keys [edge-overlapping-treshold noise-tolerance shapes]}]
  (let [radar-data (str->shape radar-sample)]
    (update-vals shapes
                 (fn [shape-str]
                   (detect-shape {:radar-data radar-data
                                  :shape (str->shape shape-str)
                                  :noise-tolerance noise-tolerance})))))

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
  (detect radar-sample
          {:shapes {:invader1 invader1
                    :invader2 invader2}
           :noise-tolerance 1/5
           :edge-overlapping-treshold 1/2}))

(comment
  (comment
      (shape->str (str->shape invader1)))
  (comment
    (shape-similarity
     (str->shape invader1)
     (str->shape invader1))

    (shape-similarity
     (str->shape invader1)
     (str->shape invader2)))
  (comment
    (->> (sub-shapes (shape-size (str->shape invader1))
                     (str->shape radar-sample))
         (mapv (fn [sub-shape]
                 (assoc sub-shape :similarity (float (shape-similarity (str->shape invader1)
                                                                       (:content sub-shape))))))
         (sort-by :similarity >)
         (take 5)
         (map (fn [{:keys [position content]}]
                (println "\n\n------\nat: " position "\n\n")
                (println (shape->str content)))))

    (mapv (fn [s]
            (println)
            (println s))
          (mapv shape->str
                (sub-shapes [5 5]
                            (str->shape radar-sample)))))

  (str->shape invader1))
