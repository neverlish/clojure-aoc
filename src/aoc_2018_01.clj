; solves https://adventofcode.com/2018/day/1

(ns aoc_2018_01)

(defn parse-int
  [string]
  (Integer/parseInt string))

(defn last-element-plus-nth-integer
  [elements integers index]
  (+ (or (last elements) 0) (nth integers index)))

(defn accumulation-reached-again-first
  [accumulations index integers]
  (let [value-added (last-element-plus-nth-integer accumulations integers index)]
    (if (.contains accumulations value-added)
      value-added
      (recur
        (conj accumulations value-added)
        (rem (+ index 1) (count integers))
        integers))))



(comment
  (->> (util/get-file "2018_01_input.txt")
       (clojure.string/split-lines)
       (map parse-int)
       (reduce +))
  (->> (util/get-file "2018_01_input.txt")
       (clojure.string/split-lines)
       (map string-to-integer)
       (accumulation-reached-again-first [] 0)))
