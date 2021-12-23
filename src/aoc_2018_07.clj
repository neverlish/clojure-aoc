(ns aoc_2018_07
  (:require util
            [clojure.set :as st]
            [clojure.string :as s]))

(defn parse-line
  [row]
  (let [[_ first next] (re-matches #"Step (\S) must be finished before step (\S) can begin." row)]
    {:first first :next next}))

(def data
  (->> "2018_07_input.txt"
       util/read-file-line
       (map parse-line)))

(defn only-in-first
  "데이터의 벡터를 입력받아, 배열 중 :first에만 있고 :next에는 없는 값들을 반환한다.

  입력예시: [{:first 1 :next 2} {:first 2 :next 3} {:first 4 :next 3}]
  출력예시: {1 4}"
  [records]
  (let [first-set (set (map :first records))
        next-set (set (map :next records))]
    (st/difference first-set next-set)))

(defn append-time
  [graph]
  (let [t (- (int (first (graph :self))) 4)]
    (assoc graph :time t)))

(defn graphed
  ":first와 :next가 담긴 map의 벡터를 입력받아,
  :next가 같은 것들의 그루핑하고, 그룹키는 :self에, 그룹된 값들의 :first의 모음은 :prerequisites에 입력한다.
  :next에 없는 :first는 {:self next, :prerequisites []와 같은 형태로 반환한다.

  입력예시: ({:first C, :next A}
            {:first C, :next F} {:first A, :next B}
            {:first A, :next D} {:first B, :next E}
            {:first D, :next E}
            {:first F, :next E})

  출력예시: ({:self C, :prerequisites [] :time 63}
            {:self A, :prerequisites [C] :time 61}
            {:self F, :prerequisites [C] :time 66}
            {:self B, :prerequisites [A] :time 62}
            {:self D, :prerequisites [A] :time 64}
            {:self E, :prerequisites [B D F] :time 65})"
  [records]
  (let [grouped-next (->> records
                          (group-by :next)
                          (map (fn [[k v]] {:self k :prerequisites (mapv :first v)})))
        beginnings (->> (only-in-first records)
                        (map (fn [ch] {:self ch :prerequisites []})))]
    (->> (concat beginnings grouped-next)
         (map append-time))))

(defn decrease-time-if-worked
  "그래프와 문자열로 된 업무목록을 받아, 그래프가 업무목록에 포함될 경우에만 그래프의 타임을 줄여서 반환한다.

  입력예시: {:self C, :prerequisites [] :time 63} [C D]
  출력예시: {:self C, :prerequisites [] :time 62}"
  [worked-list graph]
  (let [worked-set (->> worked-list (map :self) set)]
    (if (worked-set (graph :self))
      (update graph :time dec)
      graph)))

(defn graph-removed-done-workers
  "그래프목록과 삭제대상을 입력받아,
  그래프 목록에서 삭제대상 자체를 삭제하고, 그래프 내부의 :prerequisites에서도 삭제한다.

  입력예시: ({:self C, :prerequisites []}
            {:self A, :prerequisites [C]}
            {:self F, :prerequisites [C]}
            {:self B, :prerequisites [A]}
            {:self D, :prerequisites [A]}
            {:self E, :prerequisites [B D F]}) [{:self C, :prerequisites []}]

  출력예시: ({:self A, :prerequisites []}
         {:self F, :prerequisites []}
         {:self B, :prerequisites [A]}
         {:self D, :prerequisites [A]}
         {:self E, :prerequisites [B D F]})"
  [graph done-workers]
  (let [time-done-workers-set (set (map :self done-workers))]
    (->> graph
         (remove (set done-workers))
         (map #(assoc % :prerequisites (remove time-done-workers-set (% :prerequisites)))))))

(defn done-graph
  "업무자 그래프 정보와, 완료된 업무 정보를 입력받아
  1. 업무자 그래프가 완료된 업무에 포함된 경우 시간을 1 줄인다
  2. 1의 결과 중에서, 시간이 0이 된 것들을 뽑아 반환값의 :done에 포함한다.
  3. 1의 결과 중 2의 결과에 포함되는 것들은 지워서 반환값의 :graph에 포함한다

  입력예시: ({:self C, :prerequisites [] :time 1}
           {:self A, :prerequisites [C] :time 2}
           {:self F, :prerequisites [D] :time 2}
           {:self B, :prerequisites [A] :time 3}
           {:self D, :prerequisites [] :time 2}
           {:self E, :prerequisites [B D F] :time 5}) [{:self C, :prerequisites []} {:self D, :prerequisites []}]

  출력예시: ({:self A, :prerequisites [] :time 2}
           {:self F, :prerequisites [D] :time 2}
           {:self B, :prerequisites [A] :time 3}
           {:self D, :prerequisites [] :time 1}
           {:self E, :prerequisites [B D F] :time 5})"
  [workers done-list]
  (let [workers-passed-time (map #(decrease-time-if-worked done-list %) workers)
        done-workers (filter #(zero? (% :time)) workers-passed-time)
        new-graph (graph-removed-done-workers workers-passed-time done-workers)]
    {:done done-workers
     :graph new-graph}))

(defn workers-available
  "그래프, 가능 업무자 수, 그동안의 업무이력을 입력받는다.
  선결조건이 없는 업무들 중에서, 이전 업무와 이어져야 하거나 새로 시작 가능한 것들을 합쳐 가능 업무자수 만큼을 반환한다."
  [graphs worker-count worked-list]
  (let [last-worked (->> worked-list last (map :self) set)
        can-work (->> graphs
                      (filter #(empty? (% :prerequisites)))
                      (sort-by :self))
        work-continue (->> can-work
                           (filter #(last-worked (% :self))))
        new-start (->> can-work
                       (remove #(last-worked (% :self))))]
    (take worker-count (concat work-continue new-start))))

(defn progress
  [{:keys [result graphs worker-count worked-list]}]
  (let [workers (workers-available graphs worker-count worked-list)
        {new-graph :graph done-workers :done} (done-graph graphs workers)]
    {:result       (concat result done-workers)
     :graphs       new-graph
     :worker-count worker-count
     :worked-list  (merge worked-list workers)}))

(defn prepare
  [worker-count graphs]
  {:result []
   :graphs graphs
   :worker-count worker-count
   :worked-list []})

(defn result
  [graphs]
  (->> graphs
       :result
       (map :self)
       s/join))

(comment
  (->> data
       graphed
       (prepare 1)
       (iterate progress)
       (drop-while #(seq (% :graphs)))
       first
       result)
  (->> data
       graphed
       (prepare 5)
       (iterate progress)
       (drop-while #(seq (% :graphs)))
       first
       :worked-list
       count))