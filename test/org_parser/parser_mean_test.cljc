(ns org-parser.parser-mean-test
  (:require [org-parser.parser :as parser]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])))

(deftest headline
  (let [parse #(parser/org % :start :headline)]
    (testing "with crazy characters in title"
      (is (= [:headline [:stars "*****"] [:text [:text-normal "hello wörld⛄ :"]]]
             (parse "***** hello wörld⛄ :"))))))
