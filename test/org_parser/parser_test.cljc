(ns org-parser.parser-test
  (:require [org-parser.parser :as parser]
            #?(:clj [clojure.test :as t :refer :all]
               :cljs [cljs.test :as t :include-macros true])))

(deftest tags
  (let [parse #(parser/org % :start :tags)]
    (testing "single"
      (is (= [:tags "a"]
             (parse ":a:"))))
    (testing "multiple"
      (is (= [:tags "a" "b" "c"]
             (parse ":a:b:c:"))))
    (testing "with all edge characters"
      (is (= [:tags "az" "AZ" "09" "_@#%"]
             (parse ":az:AZ:09:_@#%:"))))))

(deftest headline
  (let [parse #(parser/org % :start :headline)]
    (testing "boring"
      (is (= [:headline [:stars "*"] [:title "hello" "world"]]
             (parse "* hello world"))))
    (testing "with priority"
      (is (= [:headline [:stars "**"] [:priority "A"] [:title "hello" "world"]]
             (parse "** [#A] hello world"))))
    (testing "with tags"
      (is (= [:headline [:stars "***"] [:title "hello" "world"] [:tags "the" "end"]]
             (parse "*** hello world :the:end:"))))
    (testing "with priority and tags"
      (is (= [:headline [:stars "****"] [:priority "B"] [:title "hello" "world"] [:tags "the" "end"]]
             (parse "**** [#B] hello world :the:end:"))))
    (testing "with comment flag"
      (is (= [:headline [:stars "*****"] [:comment-flag] [:title "hello" "world"]]
             (parse "***** COMMENT hello world"))))
    (testing "with crazy characters in title"
      (is (= [:headline [:stars "*****"] [:title "hello" "wörld⛄" ":"]]
             (parse "***** hello wörld⛄ :"))))))
