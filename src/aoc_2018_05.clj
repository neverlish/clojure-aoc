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

(defn removed-adjacent-insentively-same
  "삭제대상(문자열의 벡터)과 문자열을 입력하면, 문자열 내에서 근접한 두 문자가 삭제대상과 같은 경우가 없을 때까지 계속해서 삭제한다.

  입력예시: [aA Aa cC Cc] dabAcCaCBAcCcaDA
  출력예시: dabCBAcaDA"
  [to-deletes target]
  (if-let [to-remove (->> to-deletes (filter #(s/includes? target %)) first)]
    (recur to-deletes (s/replace target to-remove ""))
    target))

(defn string-char-removed
  "문자열과 지울값을 입력하면, 지울값의 대소문자 모두를 문자열에서 지운 값을 반환한다.

  입력예시: qwEeaAb e
  출력예시: qwaAb"
  [string char]
  (-> (s/replace string (s/lower-case char) "")
      (s/replace (s/upper-case char) "")))

(defn counted-per-char-removed-adjacent-insentively-same
  "입력받은 문자열에서 알파벳을 하나씩 빼고 근접한 두 문자가 같은 알파벳을 제거한 후의 길이가 가장 짧은 경우를 반환한다."
  [string]
  (let [comparator (char-couples)]
    (->> comparator
         (map #(->> (string-char-removed string (first %))
                    (removed-adjacent-insentively-same comparator)
                    count))
         sort)))

(comment
  (->> (parsed-input)
       (#(removed-adjacent-insentively-same (char-couples) %))
       count)
  (->> (parsed-input)
       counted-per-char-removed-adjacent-insentively-same
       first))