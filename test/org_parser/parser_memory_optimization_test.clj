(ns org-parser.parser-memory-optimization-test
  (:require [clojure.test :refer :all]
            [instaparse.core :as insta]
            [org-parser.parser :as parser]))

(deftest parse-defaults-to-memory-optimization
  (let [captured-args (atom nil)]
    (with-redefs [parser/parser (fn [& args]
                                  (reset! captured-args args)
                                  [:ok])]
      (parser/parse "abc" :start :text)
      (is (= ["abc" :start :text :optimize :memory]
             (vec @captured-args))))))

(deftest parse-respects-explicit-optimize-option
  (let [captured-args (atom nil)]
    (with-redefs [parser/parser (fn [& args]
                                  (reset! captured-args args)
                                  [:ok])]
      (parser/parse "abc" :start :text :optimize :speed)
      (is (= ["abc" :start :text :optimize :speed]
             (vec @captured-args))))))

(deftest text-entity-does-not-accept-empty-input
  (is (insta/failure? (parser/parse "" :start :text-entity))))

(deftest parse-memory-optimization-preserves-entity-braces
  (is (= [:text
          [:text-normal "text "]
          [:text-entity [:entity-name "Alpha"] [:entity-braces]]
          [:text-normal "followed"]]
         (parser/parse "text \\Alpha{}followed" :start :text :optimize :memory))))
