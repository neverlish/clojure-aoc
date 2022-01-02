(ns y2021.d01
  (:require [util]))

(defn data
  []
  (->> "2021/01_input.txt"
       util/read-file-line
       (map #(Integer/parseInt %))))

(comment
  (->> (data)
       (partition 2 1)
       (filter (fn [[v1 v2]] (> v2 v1)))
       count)
  (->> (data)
       (partition 3 1)
       (map #(apply + %))
       (partition 2 1)
       (filter (fn [[v1 v2]] (> v2 v1)))
       count))
