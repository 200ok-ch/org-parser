(ns org-parser.large-file-test
  (:require [clojure.string :as str]
             [clojure.test :refer :all]
            [org-parser.core :as core]
            [org-parser.parser :as parser]))

(def ^:private min-lines 31000)
(def ^:private max-elapsed-ms 30000)

(defn- line-count [s]
  (count (str/split-lines s)))

(defn- unique-suffix [copy-idx line-idx]
  (str " [copy-" copy-idx " line-" line-idx "]"))

(defn- uniquify-readme-line [line copy-idx line-idx]
  (let [suffix (unique-suffix copy-idx line-idx)]
    (cond
      (= "" line) line
      (re-matches #"\s*:END:\s*" line) line
      (re-matches #"\s*:[^:\s][^:]*:\s*" line) line
      (re-matches #"\s*#\+END[_:].*" line) line
      (re-matches #"\s*-{5,}\s*" line) line
      (re-matches #"\s*#\+BEGIN[_:].*" line) (str line suffix)
      (re-matches #"\s*[|+].*" line) line
      :else
      (if-let [[_ prefix rest] (re-matches #"(\s*#\+[A-Za-z_]+:)(.*)" line)]
        (str prefix rest suffix)
        (if-let [[_ prefix rest] (re-matches #"(\s*:[^:\s][^:]*:)(\s+.+)" line)]
          (str prefix rest suffix)
          (str line suffix))))))

(defn- repeated-readme []
  (let [readme (str (slurp "README.org") "\n")
        readme-lines (line-count readme)
        repeats (long (Math/ceil (/ min-lines (double readme-lines))))]
    {:repeats repeats
     :lines (* repeats readme-lines)
     :content (apply str (repeat repeats readme))}))

(defn- unique-repeated-readme []
  (let [readme (str (slurp "README.org") "\n")
        lines (str/split-lines readme)
        readme-lines (count lines)
        repeats (long (Math/ceil (/ min-lines (double readme-lines))))]
    {:repeats repeats
     :lines (* repeats readme-lines)
     :content (->> (for [copy-idx (range repeats)
                         [line-idx line] (map-indexed vector lines)]
                     (str (uniquify-readme-line line copy-idx line-idx) "\n"))
                   (apply str))}))

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

(deftest parse-large-unique-readme-derived-document
  (let [{:keys [repeats lines content]} (unique-repeated-readme)
        baseline (core/read-str (str (slurp "README.org") "\n"))
        expected-headline-count (* repeats (count (:headlines baseline)))
        parsed (core/read-str content)]
    (is (<= min-lines lines))
    (is (= expected-headline-count
           (count (:headlines parsed))))))

(deftest parser-accepts-large-unique-readme-derived-document
  (let [{:keys [content]} (unique-repeated-readme)]
    (is (not (parser/failure? (parser/parse content))))))
