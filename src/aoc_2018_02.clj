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

(defn common-chars-compare
  "두 문자열의 벡터를 받아, 같은 위치의 문자가 같은 문자들을 반환한다."
  [chars1 chars2]
  (reduce-kv
    (fn [result index char]
      (if (= char (nth chars2 index))
         (str result char)
         result))
    ""
    chars1))

(defn split-into-chars
  [word]
  (clojure.string/split word #""))

(defn get-longer
  [a b]
  (if (> (count a) (count b)) a b))

(defn most-common-char-in-row
  "문자열 하나와, 문자열이 든 vector을 입력받는다.
  vector 내에서 문자열과 가장 흡사한 값의 공통 문자들을 반환한다."
  [comparer rows]
  (reduce
    (fn [result word]
      (let [compared (common-chars-compare (split-into-chars comparer) (split-into-chars word))]
        (get-longer compared result)))
    ""
    rows))

(defn most-common-chars-in-rows
  "문자열의 배열을 받아, 문자열간 가장 흡사한 문자들을 반환한다."
  [rows]
  (reduce-kv
    (fn [result index row]
      (let [common-in-row (most-common-char-in-row row (subvec rows (+ index 1)))]
        (get-longer common-in-row result)))
    ""
    rows))

(comment
  (->> (util/get-file "2018_02_input.txt")
       (clojure.string/split-lines)
       (map chars-occured-2_3-counted)
       (flatten)
       (frequencies-multiplied))
  (->> (util/get-file "2018_02_input.txt")
       (clojure.string/split-lines)
       (most-common-chars-in-rows)))
