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
    (.contains visited index) :already
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
       (#(run-operations (cycle %) 0 [] (count %)))
       :result)
  (->> (data)
       rows-only-row-reversed-jmp-nop
       row-terminated-by-last-op
       :result))
