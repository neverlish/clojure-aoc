(ns aoc_2020_01
  (:require util))

(defn data
  []
  (->> "2020_01_input.txt"
       util/read-file-line
       (map #(Integer/parseInt %))))

(defn ints-2d-summed-2020
  "숫자의 벡터를 받아, 숫자의 쌍의 합이 2020이 되는 쌍을 찾는다."
  [ints]
  (for [x ints
        y ints
        :when (= 2020 (+ x y))]
    [x y]))

(comment
  (->> (data)
       (ints-2d-summed-2020)
       first
       (apply *)))