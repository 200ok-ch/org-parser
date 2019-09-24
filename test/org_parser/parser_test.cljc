(ns org-parser.parser-test
  (:require [org-parser.parser :as parser]
            #?(:clj [clojure.test :as t :refer :all]
               :cljs [cljs.test :as t :include-macros true])))


;; if parse is successful it returns a vector otherwise a map


(deftest word
  (let [parse #(parser/org % :start :word)]
    (testing "single"
      (is (= ["a"]
             (parse "a"))))
    (testing "single with trailing space"
      (is (map? (parse "ab "))))
    (testing "single with trailing newline"
      (is (map? (parse "a\n"))))))


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
    (testing "title cannot have multiple lines"
      (is (map? (parse "* a\nb"))))
    (testing "with comment flag"
      (is (= [:headline [:stars "*****"] [:comment-flag] [:title "hello" "world"]]
             (parse "***** COMMENT hello world"))))))


(deftest content
  (let [parse #(parser/org % :start :content)]
    (testing "boring"
      (is (= [:content "anything" "goes"]
             (parse "anything\ngoes"))))))


(deftest sections
  (let [parse #(parser/org % :start :sections)]
    (testing "boring"
      (is (= [:sections
              [:section
               [:headline [:stars "*"] [:title "hello" "world"]]
               [:content "this is the first section"]]
              [:section
               [:headline [:stars "**"] [:title "and" "this"]]
               [:content "is another section"]]]
             (parse "* hello world
this is the first section
** and this
is another section"))))
    (testing "boring with empty lines"
      (is (= [:sections
              [:section
               [:headline [:stars "*"] [:title "hello" "world"]]
               [:content "this is the first section"]
               [:emptyline]]
              [:section
               [:headline [:stars "**"] [:title "and" "this"]]
               [:content [:emptyline] "is another section"]]]
             (parse "* hello world
this is the first section

** and this

is another section"))))))
