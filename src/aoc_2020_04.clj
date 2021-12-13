(ns aoc_2020_04
  (:require util
            [clojure.string :as s]))

(def keys-required [:byr :iyr :eyr :hgt :hcl :ecl :pid])

(defn grouped
  "문자열을 여러 줄 받아서, 빈 행을 기준으로 잘라서 묶는다."
  [lines]
  (->> lines
       (partition-by #(= "" %))
       (map #(s/join " " %))
       (filter not-empty)))

(defn parse-row
  "콜론 앞뒤로 키와 값이 있는 쌍들이 담긴 문자열을 받아, 키워드와 값을 가진 맵으로 변환한다.

  입력예시: eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
  출력예시: {:eyr \"1972\", :cid \"100\\nhcl\", :ecl \"amb\", :hgt \"170\", :pid \"186cm\", :iyr \"2018\", :byr \"1926\"}"
  [row]
  (->> row
       (#(s/split % #" "))
       (map #(s/split % #":"))
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))

(defn parsed-file
  []
  (->> "2020_04_input.txt"
       util/read-file-line
       grouped
       (map parse-row)))

(comment
  (->> (parsed-file)
       (filter #(every? % (set keys-required)))
       count))
