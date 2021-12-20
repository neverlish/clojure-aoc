(ns aoc_2020_01
  (:require util
            [clojure.math.combinatorics :as combo]
            util))

(defn data
  []
  (->> "resources/2020_01_input.txt"
       util/read-file-line
       (map #(Integer/parseInt %))))

(defn all-different-and-summed-2020
  [args]
  (cond
    (not= (count (set args)) (count args)) false
    (= (apply + args) 2020) true
    :default false))

(defn first-row-multiplied
  [rows]
  (->> rows
       first
       (apply *)))

(defn combi
  [n rows]
  (combo/combinations rows n))

(comment
  (->> (data)
       (combi 2)
       (filter all-different-and-summed-2020)
       first-row-multiplied)
  (->> (data)
       (combi 3)
       (filter all-different-and-summed-2020)
       first-row-multiplied))