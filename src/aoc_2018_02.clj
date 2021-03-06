(ns aoc_2018_02
  (:require util))

(defn key-2-or-3?
  "hashset의 key가 2 혹은 3인지 검사한다.
  입력예시: {2 [[d 2] [j 2]]}
  출력예시: true"
  [[k v]]
  (contains? #{2 3} k))

(defn chars-occured-2_3-counted
  "문자열을 받아, 문자열내에 2번 혹은 3번 출현하는 문자가 있는지 검사한다.
  - 2번 출현하는 문자와 3번 출현하는 문자가 모두 있으면 (2 3)
  - 2번 출현하는 문자만 있으면 (2)
  - 3번 출현하는 문자만 있으면 (3)
  - 조건에 해당하는 문자가 없으면 ()
  를 반환한다.

  입력예시: fonbdmjyqujsapeczikghtvdxl
  출력예시: (2)"
  [row]
  (->> row
       frequencies
       (group-by val)
       (filter key-2-or-3?)
       keys))

(defn frequencies-multiplied
  "sequence 내부 요소들의 빈도수를 확인후 모두 곱한다.

  입력예시: (2 2 2 3 3 4 4)
  출력예시: 12"
  [elements]
  (->> elements
       frequencies
       vals
       (reduce *)))

(defn common-chars-compare
  "두 문자열의 벡터를 받아, 같은 위치의 문자가 같은 문자들을 반환한다.
  두 문자열이 같은 경우는 무시하기 위해, 무조건 빈 문자열을 반환한다.

  입력예시: abcde abfde
  출력예시: abde"
  [chars1 chars2]
  (if (= chars1 chars2)
    ""
    (->> (map vector chars1 chars2)
         (filter (fn [[a b]] (= a b)))
         (map first)
         (apply str))))

(defn most-common-chars-in-rows
  "문자열의 배열을 받아, 문자열간 가장 흡사한 문자들을 반환한다.

  입력예시: [abcde abdde abfce]
  출력예시: abde"
  [rows]
  (->> (for [row1 rows row2 rows]
         (common-chars-compare row1 row2))
       (sort-by count)
       last))

(comment
  (->> (util/get-file "2018_02_input.txt")
       clojure.string/split-lines
       (map chars-occured-2_3-counted)
       flatten
       frequencies-multiplied)
  (->> (util/get-file "2018_02_input.txt")
       clojure.string/split-lines
       most-common-chars-in-rows))
