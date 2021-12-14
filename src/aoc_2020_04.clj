(ns aoc_2020_04
  (:require util
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]))

(def keys-required #{:byr :iyr :eyr :hgt :hcl :ecl :pid})

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

(defn int-in-range?
  "특정 숫자가 특정 범위 안에 있는지 검사한다."
  [i start end]
  (let [parsed (Integer/parseInt i)]
    (<= start parsed end)))

(defn unit-int-in-range?
  "단위와 값이 함께 담긴 값을 받아, 단위별 유효범위 내에 있는지 검사한다."
  [v]
  (let [[_ i unit] (re-find #"(\d+)(cm|in)" v)]
    (case unit
      "cm" (int-in-range? i 150 193)
      "in" (int-in-range? i 59 76)
      false)))

(spec/def :passport/byr #(int-in-range? % 1920 2002))
(spec/def :passport/iyr #(int-in-range? % 2010 2020))
(spec/def :passport/eyr #(int-in-range? % 2020 2030))
(spec/def :passport/hgt #(unit-int-in-range? %))
(spec/def :passport/hcl #(re-matches #"\#[0-9a-f]{6}" %))
(spec/def :passport/ecl #(contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %))
(spec/def :passport/pid #(re-matches #"\d{9}" %))

(spec/def :passport/valid (spec/keys :req-un [:passport/byr :passport/iyr :passport/eyr :passport/hgt :passport/hcl :passport/ecl :passport/pid]
                                     :opt-un [:passport/cid]))

(comment
  (->> (parsed-file)
       (filter #(every? % keys-required))
       count)
  (->> (parsed-file)
       (filter #(spec/valid? :passport/valid %))
       count))
