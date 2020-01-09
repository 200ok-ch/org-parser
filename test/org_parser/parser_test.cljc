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
  (let [parse #(parser/org % :start :head-line)]
    (testing "boring"
      (is (= [:head-line [:stars "*"] [:title "hello" "world"]]
             (parse "* hello world"))))
    (testing "with priority"
      (is (= [:head-line [:stars "**"] [:priority "A"] [:title "hello" "world"]]
             (parse "** [#A] hello world"))))
    (testing "with tags"
      (is (= [:head-line [:stars "***"] [:title "hello" "world"] [:tags "the" "end"]]
             (parse "*** hello world :the:end:"))))
    (testing "with priority and tags"
      (is (= [:head-line [:stars "****"] [:priority "B"] [:title "hello" "world"] [:tags "the" "end"]]
             (parse "**** [#B] hello world :the:end:"))))
    (testing "title cannot have multiple lines"
      (is (map? (parse "* a\nb"))))
    (testing "with comment flag"
      (is (= [:head-line [:stars "*****"] [:comment-token] [:title "hello" "world"]]
             (parse "***** COMMENT hello world"))))))


;; (deftest content
;;   (let [parse #(parser/org % :start :content-line)]
;;     (testing "boring"
;;       (is (= [[:content-line "anything"]
;;               [:content-line "goes"]]
;;              (parse "anything\ngoes"))))))


(deftest sections
  (let [parse parser/org]
    (testing "boring"
      (is (= [:S
              [:head-line [:stars "*"] [:title "hello" "world"]]
              [:content-line "this is the first section"]
              [:head-line [:stars "**"] [:title "and" "this"]]
              [:content-line "is another section"]]
             (parse "* hello world
this is the first section
** and this
is another section"))))
    (testing "boring with empty lines"
      (is (=[:S
             [:head-line [:stars "*"] [:title "hello" "world"]]
             [:content-line "this is the first section"]
             [:empty-line]
             [:head-line [:stars "**"] [:title "and" "this"]]
             [:empty-line]
             [:content-line "is another section"]]
            (parse "* hello world
this is the first section

** and this

is another section"))))))


(deftest affiliated-keyword
  (let [parse #(parser/org % :start :affiliated-keyword-line)]
    (testing "header"
      (is (= [:affiliated-keyword-line [:key "HEADER"] [:value "hello world"]]
             (parse "#+HEADER: hello world"))))
    (testing "name"
      (is (= [:affiliated-keyword-line [:key "NAME"] [:value "hello world"]]
             (parse "#+NAME: hello world"))))
    (testing "PLOT"
      (is (= [:affiliated-keyword-line [:key "PLOT"] [:value "hello world"]]
             (parse "#+PLOT: hello world"))))
    (testing "results"
      (is (= [:affiliated-keyword-line [:key "RESULTS"] [:value "hello world"]]
             (parse "#+RESULTS: hello world"))))
    (testing "results"
      (is (= [:affiliated-keyword-line [:key "RESULTS" [:optional "asdf"]] [:value "hello world"]]
             (parse "#+RESULTS[asdf]: hello world"))))
    (testing "caption"
      (is (= [:affiliated-keyword-line [:key "CAPTION"] [:value "hello world"]]
             (parse "#+CAPTION: hello world"))))
    (testing "caption"
      (is (= [:affiliated-keyword-line [:key "CAPTION" [:optional "qwerty"]] [:value "hello world"]]
             (parse "#+CAPTION[qwerty]: hello world"))))))


(deftest todo
  (let [parse #(parser/org % :start :todo-line)]
    (testing "todos"
      (is (= [:todo-line [:todo-state "TODO"] [:done-state "DONE"]]
             (parse "#+TODO: TODO | DONE"))))))


(deftest greater-block-begin
  (let [parse #(parser/org % :start :greater-block-begin-line)]
    (testing "greater-block-begin"
      (is (= [:greater-block-begin-line [:greater-block-name "CENTER"] [:greater-block-parameters "some params"]]
             (parse "#+BEGIN_CENTER some params"))))))

(deftest greater-block-end
  (let [parse #(parser/org % :start :greater-block-end-line)]
    (testing "greater-block-end"
      (is (= [:greater-block-end-line [:greater-block-name "CENTER"]]
             (parse "#+END_CENTER"))))))
