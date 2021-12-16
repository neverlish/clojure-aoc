(ns aoc_2018_05
  (:require util
            [clojure.string :as s]))

(defn parsed-input
  []
  (->> "2018_05_input.txt"
       util/read-file-line
       first))

(defn char-set
  "글자를 받으면, 대-소 소-대 문자의 쌍을 반환한다.

  입력예시: a
  출력예시: [aA Aa]"
  [ch]
  (let [lower (str ch)
        upper (s/upper-case lower)]
    [(str lower upper) (str upper lower)]))

(defn char-couples
  "a~z 까지 대-소 소-대 문자의 쌍이 모두 담긴 벡터를 반환한다."
  []
  (->> (range 97 123)
       (map char)
       (map char-set)
       (apply concat)))

(defn removed-adjacent-insensitively-same
  "삭제대상(문자열의 벡터)과 문자열을 입력하면, 문자열 내에서 근접한 두 문자가 삭제대상과 같은 경우가 없을 때까지 계속해서 삭제한다.

  입력예시: [aA Aa cC Cc] dabAcCaCBAcCcaDA
  출력예시: dabCBAcaDA"
  [to-deletes target]
  (if-let [to-remove (->> to-deletes (filter #(s/includes? target %)) first)]
    (recur to-deletes (s/replace target to-remove ""))
    target))

(defn string-char-removed-and-adjacent-insensitively-same-count
  "입력받은 문자열에서 알파벳에 해당하는 글자들을 지우고, 근접한 두 문자 같은 것들을 제거한 글자의 글자수를 반환한다.

  입력예시: qwEeaAb e
  출력예시: 5"
  [string char comparator]
  (->>  (str "(?i)" char)
        re-pattern
        (#(s/replace string % ""))
        (removed-adjacent-insentively-same comparator)
        count))

(defn counted-per-char-removed-adjacent-insensitively-same
  "입력받은 문자열에서 알파벳을 하나씩 빼고 근접한 두 문자가 같은 알파벳을 제거한 후의 길이가 가장 짧은 경우를 반환한다."
  [string]
  (let [comparator (char-couples)]
    (map
      #(string-char-removed-and-adjacent-insensitively-same-count string (first %) comparator)
      comparator)))

(comment
  (->> (parsed-input)
       (removed-adjacent-insensitively-same (char-couples))
       count)
  (->> (parsed-input)
       counted-per-char-removed-adjacent-insensitively-same
       sort
       first))