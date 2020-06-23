(ns org-parser.parser-mean-test
  (:require [org-parser.parser :as parser]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])))

(deftest headline
  (let [parse #(parser/org % :start :head-line)]
    (testing "with crazy characters in title"
      (is (= [:head-line [:stars "*****"] [:title "hello" "wörld⛄" ":"]]
             (parse "***** hello wörld⛄ :"))))))
