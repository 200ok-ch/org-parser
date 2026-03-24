(ns org-parser.parser
  (:require [org-parser.antlr.parser :as antlr]))

(defn- option-value [options k default]
  (let [pairs (partition 2 options)
        found (reduce (fn [acc [k* v]]
                        (if (and (= acc ::not-found) (= k k*))
                          v
                          acc))
                      ::not-found
                      pairs)]
    (if (= found ::not-found) default found)))

(defn- parse-antlr [raw start]
  (let [antlr-start (keyword (name start))]
    (-> (antlr/parse raw {:start antlr-start})
        (vary-meta merge {:raw raw :backend-used :antlr}))))

(defn- normalize-top-level-s [ast]
  (if (and (sequential? ast)
           (= :S (first ast)))
    (or (some #(when (and (sequential? %)
                          (= :S (first %)))
                 %)
              (rest ast))
        ast)
    ast))

(defn- failure-result? [x]
  (true? (:failure? x)))

(defn parse [raw & options]
  (let [start (option-value options :start :S)
        antlr-result (parse-antlr raw start)]
    (if (= start :S)
      (normalize-top-level-s antlr-result)
      antlr-result)))

(defn failure? [x]
  (failure-result? x))

(defn span [ast]
  (or (:span (meta ast))
      [0 0]))
