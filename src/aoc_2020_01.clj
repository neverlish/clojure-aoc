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
        :when (and (not= v1 v2) (= 2020 (+ v1 v2)))]
    [v1 v2]))

(defn vectors-int-diff-3d
  [ints]
  (for [v1 ints
        v2 ints
        v3 ints
        :when (and (not= v1 v2 v3) (= 2020 (+ v1 v2 v3)))]
    [v1 v2 v3]))

(comment
  (->> (data)
       vectors-int-diff-2d
       first
       (apply *))
  (->> (data)
       vectors-int-diff-3d
       first
       (apply *)))