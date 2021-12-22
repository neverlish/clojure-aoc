(ns aoc_2020_08
  (:require util
            [clojure.string :as st]))

(defn parse-row
  [index row]
  (let [[k v] (st/split row #" ")]
    {:index index :op (keyword k) :value (Integer/parseInt v)}))

(defn data
  []
  (->> "2020_08_input.txt"
       util/read-file-line
       (map-indexed parse-row)
       vec))

(defn append-row-by-last-op
  "명령어의 벡터와 인덱스의 벡터를 받아,
  인덱스 벡터의 마지막 인덱스에 해당하는 위치의 명령을 수행한 후,
  그 명령어의 위치를 인덱스 벡터에 추가한다."
  [rows accs]
  (let [{op :op value :value index :index} (peek accs)
        index-to-add (case op
                       :acc (inc index)
                       :jmp (+ index value)
                       :nop (inc index))
        row-to-add (nth rows index-to-add)]
    (conj accs row-to-add)))

(defn can-continue?
  "벡터와 값을 입력받아, 값이 중복되지 않으면서도 마지막의 이전값이 특정값에 해당하는를 검사한다."
  [v]
  (fn [vector]
    (and (apply distinct? vector)
         (->> vector last :index (not= v)))))

(defn operated
  "명령어들을 받은 후, 값이 중복되지 않거나 마지막 명령어가 수행될때까지 명령어들을 계속 수행한 결과 명령어들의 수행순서를 반환한다."
  [rows]
  (let [max-index (->> rows (map :index) (apply max))]
    (->> (iterate #(append-row-by-last-op rows %) [(first rows)])
         (drop-while (can-continue? max-index))
         first)))

(defn rows-summed-acc
  "명령어의 배열을 받은 후, op가 :acc 인 것들의 :value의 합을 구한다."
  [rows]
  (->> rows
       (filter #(= (% :op) :acc))
       (map :value)
       (apply +)))

(defn rows-only-row-reversed-jmp-nop
  "명령의 벡터를 받아, 명령이 :jmp 혹은 :nop 인 것들의 명령만 하나씩 변경한 2차원 벡터를 반환한다.

  입력예시: [{:op :jmp :value 1} {:op :acc :value 2} {:op :nop :value -1}]
  출력예시: [[{:op :nop :value 1} {:op :acc :value 2} {:op :nop :value -1}]
           [{:op :jmp :value 1} {:op :acc :value 2} {:op :nop :value -1}]
           [{:op :jmp :value 1} {:op :acc :value 2} {:op :jmp :value -1}]]"
  [rows]
  (for [row rows]
    (let [op-changed (case (row :op)
                       :jmp :nop
                       :nop :jmp
                       :acc :acc)]
      (assoc rows (row :index) (assoc row :op op-changed)))))

(defn row-terminated-by-last-op
  "명령의 2차원 벡터를 받아, 명령의 모음 중 마지막 명령까지 수행된 모음을 반환한다."
  [rows-2d]
  (let [last-index (->> rows-2d last last :index)]
    (->> rows-2d
         (map operated)
         (filter #(->> % last :index (= last-index)))
         first)))

(comment
  (->> (data)
       operated
       rows-summed-acc)
  (->> (data)
       rows-only-row-reversed-jmp-nop
       row-terminated-by-last-op
       rows-summed-acc))
