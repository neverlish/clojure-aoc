(ns aoc_2018_02)

(defn is-key-2-or-3?
  "hashset의 key가 2 혹은 3인지 검사한다."
  [[k v]]
  (contains? #{2 3} k))

(defn chars-occured-2_3-counted
  "문자열을 받아, 문자열내에 2번 혹은 3번 출현하는 문자가 있는지 검사한다.
  - 2번 출현하는 문자와 3번 출현하는 문자가 모두 있으면 (2 3)
  - 2번 출현하는 문자만 있으면 (2)
  - 3번 출현하는 문자만 있으면 (3)
  - 조건에 해당하는 문자가 없으면 ()
  를 반환한다."
  [row]
  (->> row
       (frequencies)
       (group-by val)
       (filter is-key-2-or-3?)
       (keys)))

(defn frequencies-multiplied
  "sequence 내부 요소들의 빈도수를 확인후 모두 곱한다."
  [elements]
  (->> elements
       (frequencies)
       (vals)
       (reduce *)))

(comment
  (->> (util/get-file "2018_02_input.txt")
       (clojure.string/split-lines)
       (map chars-occured-2_3-counted)
       (flatten)
       (frequencies-multiplied)))

