(ns y2021.d03
  (:require [util]
            [clojure.string :as s]))

(defn parse
  [string]
  (let [splitted (s/split string #"")]
    (map #(Integer/parseInt %) splitted)))

(defn data
  []
  (->> "2021/03_input.txt"
       util/read-file-line
       (map parse)))

(defn pivot
  [matrix]
  (apply map vector matrix))

(defn most-least
  [ints]
  (let [freq (->> ints
                  frequencies
                  (sort-by (juxt val key)))]
    {:most (->> freq last key)
     :least (->> freq first key)}))

(defn binary-join-parse
  [ints]
  (Integer/parseInt (s/join ints) 2))

(defn most-least-mapped
  [freqs]
  {:most (map :most freqs)
   :least (map :least freqs)})

(defn filter-by-key
  [freq-key]
  (fn [{:keys [matrix index]}]
    (let [comparator (->> matrix
                          (map #(nth % index))
                          most-least
                          freq-key)
          new-matrix (filter #(= (nth % index) comparator) matrix)]
      {:matrix new-matrix
       :index (inc index)})))

(defn rate
  [key matrix]
  (->> {:matrix matrix :index 0}
       (iterate (filter-by-key key))
       (drop-while #(> (count (% :matrix)) 1))
       first
       :matrix
       first))

(defn ratings
  [matrix]
  {:oxygen (rate :most matrix)
   :co2 (rate :least matrix)})

(defn vals-multiplied
  [kv]
  (->> kv
       vals
       (map binary-join-parse)
       (apply *)))

(comment
  (->> (data)
       pivot
       (map most-least)
       most-least-mapped
       vals-multiplied)
  (->> (data)
       ratings
       vals-multiplied))