(ns y2021.d02
  (:require [util]
            [clojure.string :as s]))

(defn parse
  [row]
  (let [[op value] (s/split row #" ")]
    {:op (keyword op) :value (Integer/parseInt value)}))

(defn data
  []
  (->> "2021/02_input.txt"
       util/read-file-line
       (map parse)))

(defn progress1
  [{:keys [x depth]} {:keys [op value]}]
  (case op
    :forward {:x (+ x value)
              :depth depth}
    :down {:x x
           :depth (+ depth value)}
    :up {:x x
         :depth (- depth value)}))

(defn progress2
  [{:keys [x depth aim]} {:keys [op value]}]
  (case op
    :forward {:x (+ x value)
              :depth (+ depth (* aim value))
              :aim aim}
    :down {:x x
           :depth depth
           :aim (+ aim value)}
    :up {:x x
         :depth depth
         :aim (- aim value)}))

(defn result
  [{:keys [x depth]}]
  (* x depth))

(def initial
  {:x 0 :depth 0 :aim 0})

(comment
  (->> (data)
       (reduce progress1 initial)
       result)
  (->> (data)
       (reduce progress2 initial)
       result))