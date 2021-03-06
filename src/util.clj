(ns util
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn get-file
  [filename]
  (->> filename
       (str "resources/")
       slurp))

(defn read-file-line
  [filename]
  (->> filename
       get-file
       s/split-lines))
