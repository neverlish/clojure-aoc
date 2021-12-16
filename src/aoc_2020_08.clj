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

(defn append-index-by-last-op
  "명령어의 벡터와 인덱스의 벡터를 받아,
  인덱스 벡터의 마지막 인덱스에 해당하는 위치의 명령을 수행한 후,
  그 명령어의 위치를 인덱스 벡터에 추가한다."
  [rows indices]
  (let [{op :op value :value index :index} (rows (peek indices))
        index-to-add (case op
                       :acc 1
                       :jmp value
                       :nop 1)]
    (conj indices (+ index index-to-add))))

(defn rows-until-condition
  "명령어들을 받은 후, 특정 조건에 해당할 때까지 명령어들을 계속 수행한 결과 명령어들의 수행순서를 반환한다."
  [until]
  (fn [rows]
    (->> (iterate #(append-index-by-last-op rows %) [0])
         (take-while until)
         last
         (map rows))))

(defn rows-summed-acc
  "명령어의 배열을 받은 후, op가 :acc 인 것들의 :value의 합을 구한다."
  [rows]
  (->> rows
       (filter #(= (% :op) :acc))
       (map :value)
       (apply +)))

(defn do-operation
  "명령어 와 값들을 받아, 전진수와 더할값을 반환한다."
  [op v whole-count]
  (case op
    :acc {:step 1 :add v}
    :jmp {:step (if (pos? v) v (+ v whole-count))
          :add 0}
    :nop {:step 1 :add 0}))

(defn reason-terminated
  "명령의 종료사유를 반한한다.
  마지막 명령일 경우 :last-op 을, 기방문한 명령인 경우 :already를 반환한다.
  해당하는 조건이 없는 경우 nil을 반환한다.

  입력예시: [[0 2 3] 1 4]
  출력예시: :last-op"
  [visited index whole-count]
  (cond
    (= (last visited) (dec whole-count)) :last-op
    (contains? (set visited) index) :already
    :default nil))

(defn run-operations
  "명령어의 벡터, 결과, 방문여부, 명령어의 전체길이를 입력 받아
  명령의 종료조건과 최종결과를 반환한다."

  [ops result visited whole-count]
  (let [{op :op value :value index :index} (first ops)
        termination (reason-terminated visited index whole-count)]
    (if termination
      {:termination termination :result result}
      (let [{step :step add :add} (do-operation op value whole-count)]
        (recur (nthrest ops step)
               (+ result add)
               (conj visited index)
               whole-count)))))

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
  "명령의 벡터를 받아, 종료조건이 :last-op인 첫번째 명령을 반환한다."
  [rows]
  (->> rows
       (map #(run-operations (cycle %) 0 [] (count %)))
       (filter #(= (% :termination) :last-op))
       first))

(comment
  (->> (data)
       ((rows-until-condition #(apply distinct? %)))
       rows-summed-acc)
  (->> (data)
       rows-only-row-reversed-jmp-nop
       row-terminated-by-last-op
       :result))
