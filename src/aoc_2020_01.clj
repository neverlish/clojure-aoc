(ns aoc_2020_01
  (:require util))

(defn data
  []
  (->> "2020_01_input.txt"
       util/read-file-line
       (map #(Integer/parseInt %))))

(defn vectors-int-diff-2d
  [ints]
  (for [v1 ints
        v2 ints
        :when (not= v1 v2)]
    [v1 v2]))

(defn vectors-int-diff-3d
  [ints]
  (for [v1 ints
        [v2 v3] (vectors-int-diff-2d ints)
        :when (and (not= v1 v2) (not= v1 v3))]
    [v1 v2 v3]))

(defn row-summed-to-2020
  [rows]
  (->> rows
       (filter #(= (apply + %) 2020))
       first))

(comment
  (->> (data)
       vectors-int-diff-2d
       row-summed-to-2020
       (apply *))
  (->> (data)
       vectors-int-diff-3d
       row-summed-to-2020
       (apply *)))