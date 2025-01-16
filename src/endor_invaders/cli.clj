(ns endor-invaders.cli
  (:require [endor-invaders.core :as core]
            [malli.core :as malli]
            [malli.error :as me]))

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

(defn print-detections
  "Pretty print the output of the `endor-invaders.core/detect` function."
  [detections]
  (println)
  (mapv (fn [[type results]]
          (when (seq results)
            (println "------------------------")
            (println "DETECTED: " type)
            (mapv (fn [{:keys [position content similarity]}]
                    (println
                     (str "\nat: " position
                          "\nprob: " (format "%.2f" (float similarity))
                          "\n::\n"
                          (core/matrix->str content)
                          "\n")))
                  results)))
        detections))

(def ARGUMENTS_SCHEMA

  [:map

   [:radar-data
    {:description "A filepath pointing to some radar data."}
    [:and
     [:or symbol? string?]
     [:fn {:error/message "Should point to a file."}
      #(.exists (java.io.File. (str %)))]]]

   [:similarity-threshold
    {:description "A number between 0 and 1"}
    [:and
     number?
     [:fn {:error/message "Should be in the 0..1 range"}
      #(<= 0 % 1)]]]])

(defn detect-invaders
  "cli entry point"
  [{:as args :keys [radar-data similarity-threshold]}]
  (if (malli/validate ARGUMENTS_SCHEMA args)
    (let [filepath (str radar-data)]
      (-> (core/detect (slurp filepath)
                       {:shapes {:invader1 invader1
                                 :invader2 invader2}
                        :similarity-threshold similarity-threshold})
          (try (catch Exception e (println "malformed radar data:\n" (.getMessage e))))
          print-detections))
    (-> (malli/explain ARGUMENTS_SCHEMA args)
        (me/humanize)
        (println))))
