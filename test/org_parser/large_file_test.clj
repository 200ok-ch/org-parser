(ns org-parser.large-file-test
  (:require [clojure.string :as str]
             [clojure.test :refer :all]
            [org-parser.core :as core]
            [org-parser.parser :as parser]))

(def ^:private min-lines 31000)
(def ^:private max-elapsed-ms 30000)

(defn- line-count [s]
  (count (str/split-lines s)))

(defn- repeated-readme []
  (let [readme (str (slurp "README.org") "\n")
        readme-lines (line-count readme)
        repeats (long (Math/ceil (/ min-lines (double readme-lines))))]
    {:repeats repeats
     :lines (* repeats readme-lines)
     :content (apply str (repeat repeats readme))}))

(deftest parse-large-readme-derived-document
  (let [{:keys [repeats lines content]} (repeated-readme)
        baseline (core/read-str (str (slurp "README.org") "\n"))
        expected-headline-count (* repeats (count (:headlines baseline)))
        start (System/nanoTime)
        parsed (core/read-str content)
        elapsed-ms (/ (- (System/nanoTime) start) 1000000.0)]
    (is (<= min-lines lines))
    (is (= expected-headline-count
           (count (:headlines parsed))))
    (is (< elapsed-ms max-elapsed-ms)
        (str "parsing took " elapsed-ms "ms, expected < " max-elapsed-ms "ms"))))

(deftest parser-accepts-large-readme-derived-document
  (let [{:keys [content]} (repeated-readme)]
    (is (not (parser/failure? (parser/parse content))))))
