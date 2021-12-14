(ns aoc_2018_06
  (:require util
            [clojure.string :as s]))

(defn row-parsed
  "문자열과 문자열의 줄수를 받아, 좌표정보로 변환한다.

  입력예시: [3 1,2]
  출력예시: {:idx 3 :x 1 :y 2}"
  [idx row]
  (let [[x y] (s/split row #", ")]
    {:idx (str idx) :x (Integer/parseInt x) :y (Integer/parseInt y)}))

(defn data
  []
  (->> "2018_06_input.txt"
       util/read-file-line
       (map-indexed row-parsed)))

(defn distance
  "두 좌표간의 거리를 구한다."
  [{target-x :x target-y :y} {self-x :x self-y :y}]
  (+ (Math/abs(- target-x self-x))
     (Math/abs(- self-y target-y))))

(defn on-border?
  "좌표가 평면의 경계선 위에 있는지 확인한다."
  [self max-x max-y]
  (or (contains? #{0 max-x} (self :x))
      (contains? #{0 max-y} (self :y))))

(defn dot-type-closest-idx
  "좌표와 근접좌표 배열을 받아,

  최근접좌표가 2개 이상이면 :duplicated를,
  1개이면 최근접좌표의 :idx를 :type에 붙여 반환한다.."
  [self [coord1 coord2]]
  (let [type (if (= (distance self coord1) (distance self coord2))
               :duplicated
               (coord1 :idx))]
      (assoc self :type type)))

(defn max-dot-coordinate
  "좌표배열을 받아, x와 y의 최고값이 담긴 배열을 반환한다."
  [coords]
  [(->> coords (map :x) (apply max))
   (->> coords (map :y) (apply max))])

(defn dots-marked-closest-index
  "좌표배열을 받아 평면을 생성후, 평면 내부의 점 별로 최근접좌표 정보를 붙여 반환한다."
  [coords]
  (let [[max-x max-y] (max-dot-coordinate coords)]
    (for [x (range 0 (inc max-x))
          y (range 0 (inc max-y))]
      (->> coords
           (sort-by #(distance {:x x :y y} %))
           (dot-type-closest-idx {:x x :y y})))))

(defn valid-coords
  "점이 담긴 배열 중, 최근접좌표가 중복되지 않고, 경계선에 있지 않은 점들만 반환한다."
  [coords]
  (let [[max-x max-y] (max-dot-coordinate coords)]
    (->> coords
         (group-by :type)
         (filter (fn [[k v]]
                   (not (= k :duplicated))
                   (not (some #(on-border? % max-x max-y) v)))))))

(defn most-frequent-idx-count
  "좌표가 key, 좌표배열이 value인 map을 입력받아,

  value의 요소가 가장 긴 것의 길이를 반환한다."
  [coords]
  (->> coords
       (map #(count (last %)))
       sort
       last))

(defn distance-summed-per-dots
  "좌표정보를 이용하여 평면을 생성후, 평면 상의 점 별로 좌표와의 거리합을 구한다."
  [coords]
  (let [[max-x max-y] (all-dots coords)]
    (for [x (range 0 (inc max-x))
          y (range 0 (inc max-y))]
      (->> coords
           (map #(distance {:x x :y y} %))
           (apply +)))))

(comment
  (->> (data)
       dots-marked-closest-index
       valid-coords
       most-frequent-idx-count)
  (->> (data)
       distance-summed-per-dots
       (filter #(> 10000 %))
       count))
