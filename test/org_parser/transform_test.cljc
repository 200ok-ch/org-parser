(ns org-parser.transform-test
  (:require [org-parser.transform :as sut]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer :all :include-macros true])))


(def props
  [[:stars "*"] [:title "hello" "world"]])


(deftest property
  (testing "helper fn"
    (is (= '("hello" "world")
           (#'sut/property :title props)))))


(deftest append-content
  (testing "already in content block"
    (is (= [[:content "hello\nworld\n"]]
         (#'sut/append-content [[:content "hello\n"]] "world")))))


(deftest reducer
  (testing "content-line begins block"
    (is (= [[:headline] [:content "hello world\n"]]
         (sut/reducer [[:headline]] [:content-line "hello world"])))))


(def parse-tree
  [:S
   [:head-line [:stars "*"] [:title "hello" "world"]]
   [:content-line "this is the first section"]
   [:empty-line]
   [:head-line [:stars "**"] [:title "and" "this"]]
   [:empty-line]
   [:content-line "is another section"]])


(deftest x
  (testing "y"
    (is (= [[:headline {:level 1, :title "hello world"}]
           [:content "this is the first section\n\n"]
           [:headline {:level 2, :title "and this"}]
           [:content "\nis another section\n"]]
           (sut/transform parse-tree)))))
