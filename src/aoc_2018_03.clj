(ns aoc_2018_03
  (:require util
            [clojure.string :as s]))

(defn square-coordinates
  "int 의 vector가 입력되면, 의미를 가지는 hash-map으로 변환한다.
  주의: end-x 와 end-y 는 exclusive 하다.
  입력예시: [1 3 4 4]
  출력예시: {:start-x 1 :start-y 3 :end-x 5 :end-y 7}"
  [row]
  {:id (nth row 0)
   :start-x (nth row 1)
   :start-y (nth row 2)
   :end-x (+ (nth row 1) (nth row 3))
   :end-y (+ (nth row 2) (nth row 4))})

(defn row-to-coordinate
  "#1 @ 1,3: 4x4 와 같은 좌표정보가 들어오면, 좌표정보 좌상단 점과 우하단 점을 확인할 수 있는 hash-map을 반환한다.
  #1 @ 1,3: 4x4 에서 #1은 줄별 id. @ 1,3 은 시작점. 4x4 는 정사각형의 크기를 가리킨다.
  입력예시: #1 @ 1,3: 4x4
  출력예시: {:id 1 :start-x 1 :start-y 3 :end-x 5 :end-y 7}"
  [row]
  (->> (s/split row #"\#| @ |,|: |x")
       rest
       (map #(Integer/parseInt %))
       square-coordinates))

(defn max-val
  "hash-map의 배열 에서, 특정 키의 최고값을 반환한다.
  입력예시:
    rows: ({:start-x 1, :start-y 3, :end-x 5, :end-y 7} {:start-x 3, :start-y 1, :end-x 7, :end-y 5})
    k: :start-x
  출력예시: 3"
  [rows k]
  (get (apply max-key k rows) k))


(defn entire-coordinates
  "사각형들의 배열을 받아, 가장 큰 end-x/end-y 값이 담긴 정보를 추가로 덧붙인다.
  입력예시: ({:start-x 1, :start-y 3, :end-x 5, :end-y 7} {:start-x 3, :start-y 1, :end-x 7, :end-y 5})
  출력예시: {:entire {:x 7 :y 7} :squares: ({:start-x 1, :start-y 3, :end-x 5, :end-y 7} {:start-x 3, :start-y 1, :end-x 7, :end-y 5})}"
  [squares]
  {:entire  {:x (max-val squares :end-x) :y (max-val squares :end-y)}
   :squares squares})

(defn coordinate-within-square?
  "좌표 사각형 정보와 x y 값이 주어지면, x y 가 사각형 안에 속하는지 판단한다.
  입력예시:
    coor: {:start-x 1, :start-y 3, :end-x 5, :end-y 7}
    x: 4
    y: 6
  출력예시: true"
  [x y square]
  (and (<= (square :start-x) x)
       (<= (square :start-y) y)
       (> (square :end-x) x)
       (> (square :end-y) y)))

(defn squares-id-not-overlapped
  [squares]
  (case (count squares)
    1 (get (first squares) :id)
    0 :none
    :over-one))

(defn claims-formatted
  "사각형들의 배열 정보를 받아, 사각형들의 좌표평면 내에서의 위치정보 등을 활용한 고차함수를 실행한다."
  [format]
  (fn [{entire :entire squares :squares}]
    (for [x (range (entire :x))
          y (range (entire :y))]
      (->> squares
           (filter #(coordinate-within-square? x y %))
           format))))

(defn square-area
  "사각형 정보를 받으면, 면적 정보를 추가하여 반환한다.
  입력예시: {:start-x 1, :start-y 3, :end-x 5, :end-y 7}
  출력예시: {:start-x 1, :start-y 3, :end-x 5, :end-y 7 :area 16}"
  [square]
  (let [area (* (- (square :end-x) (square :start-x))
                (- (square :end-y) (square :start-y)))]
    (assoc square :area area)))

(defn squares-id-not-overlapped-frequencies
  "사각형 배열의 정보를 입력받으면, 겹치지 않는 사각형들의 좌표별 id 출현횟수를 반환한다.
  입력예시: {:entire {:x 7 :y 7} :squares '({:id 1 :start-x 1, :start-y 3, :end-x 5, :end-y 7} {:id 2 :start-x 3, :start-y 1, :end-x 7, :end-y 5})}
  출력예시: {:none 21, 1 12, 2 12, :over-one 4}"
  [squres-and-entire]
  (->> squres-and-entire
       ((claims-formatted squares-id-not-overlapped))
       frequencies))

(defn filter-area-same-frequencies
  "areas 에서의 면적 정보와, freq에서 id에 해당하는 빈도수가 같은 areas들만 반환한다."
  [areas freqs]
  (filter
    (fn [{area :area id :id}]
      (= area (get freqs id)))
    areas))

(defn squares-not-overlapped
  "사각형 중 다른 사각형의 침범을 받지 않은 사각형들만 반환한다."
  [squres-and-entire]
  (let [square-not-overlapped-frequencies (squares-id-not-overlapped-frequencies squres-and-entire)
        areas (map square-area (squres-and-entire :squares))]
    (filter-area-same-frequencies areas square-not-overlapped-frequencies)))

(comment
  (->> "2018_03_input.txt"
       util/get-file
       s/split-lines
       (map row-to-coordinate)
       entire-coordinates
       ((claims-formatted count))
       (filter #(> % 1))
       count)
  (->> "2018_03_input.txt"
       util/get-file
       s/split-lines
       (map row-to-coordinate)
       entire-coordinates
       squares-not-overlapped))
