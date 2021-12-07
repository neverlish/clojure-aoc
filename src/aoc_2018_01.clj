; solves https://adventofcode.com/2018/day/1

(ns aoc_2018_01
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn get-file
  [filename]
  (->> filename
       (str "file/")
       (io/resource)
       (slurp))
  )

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s )))

(defn string-to-integer
  [string]
  (if (= (first string) \-)
    (* (parse-int (subs string 1)) -1)
    (parse-int (subs string 1))
    )
  )

(defn split
  [strings]
  (str/split strings #"(\n)|(, )")
  )

(defn sum-strings
  [strings]
  (->> strings
       (split)
       (map string-to-integer)
       (reduce +))
  )

(comment
  (->> (get-file "2018_01_input.txt")
       (sum-strings)))