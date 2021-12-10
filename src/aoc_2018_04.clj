(ns aoc_2018_04
  (:require util
            [clojure.string :as s]))

(defn row-to-data
  "문자열의 배열을 입력받아, 가공하여 반환한다."
  [[_ _ _ _ _ minute info1 info2]]
  {:minute (Integer/parseInt minute)
   :guard-id (if (= info1 "Guard") (Integer/parseInt (subs info2 1)))})

(defn parse-row
  "시간과 정보가 담긴 문자열이 들어오면, 가공하여 반환한다.
  정보:
    :minute : 분 정도
    :guard-id : 문자열이 Guard 정보면 guard의 id를, 그렇지 않다면 nil을 입력한다.

  입력예시: [1518-06-03 00:00] Guard #659 begins shift
  출력예시: {:minute 03 :guard-id 659}"
  [row]
  (->> row
       (#(s/split % #"\[|] |-|:| "))
       row-to-data))

(defn guard-row-indices
  "문자열들을 여러줄 입력받아, Guard 정보가 담긴 줄의 줄수들만 반환한다.

  입력예시: [1518-06-03 00:00] Guard #659 begins shift\n[1518-10-03 00:35] wakes up\n[1518-04-30 00:20] wakes up\n[1518-10-15 23:59] Guard #1699 begins shift\n[1518-06-07 00:39] wakes up\n[1518-09-25 00:10] falls asleep
  출력예시: [0 3]"
  [rows]
  (->> rows
       (map #(vector %1 %2) (range (count rows)))
       (filter #(->> % last :guard-id some?))
       (map first)
       (#(map range (drop-last %) (rest %)))))


(defn minutes-to-range
  "분 정보가 담긴 맵의 벡터를 전달받아, 처음과 마지막 값을 이용한 range를 반환한다.

  입력예시: [{:minute 3} {:minute 6}]
  출력예시: (3 4 5)"
  [rows]
  (->> rows
       (map :minute)
       (#(range (first %) (last %)))))


(defn transform-group
  [group]
  {:guard-id ((first group) :guard-id)
   :minutes (->> group
                 rest
                 (partition 2)
                 (map minutes-to-range)
                 flatten)})

(defn group-by-guard-id
  "문자열을 입력받아, 줄별로 파싱한 이후, guard-id 별로 정보들을 그루핑한다."
  [rows]
  (->> rows
       guard-row-indices
       (map #(map (fn [int] (nth rows int)) %))
       (map transform-group)
       (group-by :guard-id)))

(defn slept-longest-guard
  "guard-id와 분정보가 담긴 리스트의 벡터를 받아, 가장 많이 잠을 잔 가드를 반환한다."
  [group]
  (->> group
       (sort-by #(->> % last (map :minutes) flatten count))
       last))

(defn most-frequent-minute
  "분 정보가 담긴 리스트의 벡터들을 받아, 최빈분과 빈도의 map을 반환한다.

  입력예시: [{:minutes (37 38 39 40)} {:minutes (36 37)}]
  출력예시: {37 2}"
  [minutes]
  (->> minutes
       (map :minutes)
       flatten
       frequencies
       (sort-by val)
       last))

(defn multiplied-guard-id-and-most-frequent-minute
  "guard-id와 분정보의 리스트 담긴 벡터를 받아, guard-id와 최빈분을 곱한 값을 반환한다.

  입력예시: [{:guard-id 2953, :minutes (37 38 39 40)} {:guard-id 2953, :minutes (36 37)}]
  출력예시: 109261"
  [[guard-id minutes]]
  (* guard-id (first (most-frequent-minute minutes))))

(comment
  (->> "2018_04_input.txt"
       util/read-file-line
       sort
       (map parse-row)
       group-by-guard-id
       slept-longest-guard
       multiplied-guard-id-and-most-frequent-minute))