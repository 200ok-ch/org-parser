(ns org-parser.parser-test
  (:require [org-parser.parser :as parser]
            #?(:clj [clojure.test :as t :refer :all]
               :cljs [cljs.test :as t :include-macros true])))

(deftest headline
  (let [parse #(parser/org % :start :headline)]
    (testing "with crazy characters in title"
      (is (= [:headline [:stars "*****"] [:title "hello" "wörld⛄" ":"]]
             (parse "***** hello wörld⛄ :"))))))
