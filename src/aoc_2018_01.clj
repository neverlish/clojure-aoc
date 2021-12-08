; solves https://adventofcode.com/2018/day/1

(ns aoc_2018_01)

(defn parse-int
  "입력된 문자를 숫자로 반환한다."
  [string]
  (Integer/parseInt string))

(defn peek-element
  "vector를 입력하면 마지막 요소를 반환한다. vector가 빈 경우 0을 반환한다."
  [elements]
  (or (peek elements) 0))

(defn accumulation-reached-again-first
  "integers의 누적합 중 재출현하는 첫번째 값을 계산한다.
  integers를 끝까지 누적해도 재출현하는 누적값이 없다면 기존의 누적합에 이어서 integers 의 첫번째요소부터 다시 누적한다.
  입력형태:
    accumulations: vector
    index: int
    integers: vector"
  [accumulations index integers]
  (let [value-added (+ (peek-element accumulations) (nth integers index))]
    (if (some #{value-added} accumulations)
      value-added
      (recur
        (conj accumulations value-added)
        (rem (+ index 1) (count integers))
        integers))))

(comment
  ; day1 첫번째 파트 해결
  (->> (util/get-file "2018_01_input.txt")
       (clojure.string/split-lines)
       (map parse-int)
       (reduce +))
  ; day1 두번째 파트 해결
  (->> (util/get-file "2018_01_input.txt")
       (clojure.string/split-lines)
       (map parse-int)
       (accumulation-reached-again-first [] 0)))
