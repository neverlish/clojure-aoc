(ns util
  (:require [clojure.java.io :as io]))

(defn get-file
  [filename]
  (->> filename
       (io/resource)
       (slurp)))
