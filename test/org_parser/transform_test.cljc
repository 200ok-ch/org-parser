(ns org-parser.transform-test
  (:require [org-parser.transform :as sut]
            [org-parser.parser :as parser]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer :all :include-macros true])))


(def props
  [[:stars "*"] [:title "hello" "world"]])


(deftest property
  (testing "helper fn"
    (is (= '("hello" "world")
           (#'sut/property :title props)))))


(deftest append
  (testing "already in content block"
    (is (= [[:content "hello\nworld\n"]]
           (#'sut/append [[:content "hello\n"]] "world")))))


(deftest reducer
  (testing "content-line begins block"
    (is (= [[:headline] [:content "hello world\n"]]
           (#'sut/reducer [[:headline]] [:content-line "hello world"])))))


(def parse-tree
  [:S
   [:head-line [:stars "*"] [:title "hello" "world"]]
   [:content-line "this is the first section"]
   [:empty-line]
   [:head-line [:stars "**"] [:title "and" "this"]]
   [:empty-line]
   [:content-line "is another section"]])


(def transformed
  [[:headline {:level 1, :title "hello world"}]
   [:content "this is the first section\n\n"]
   [:headline {:level 2, :title "and this"}]
   [:content "\nis another section\n"]])

(deftest regression
  (testing "`transform` works on structures provided by parser"
    (let [parse parser/org]
      (is (= parse-tree
             (parse "* hello world
this is the first section

** and this

is another section"))))))

(deftest transform
  (testing "a full parsetree"
    (is (= transformed
           (sut/transform parse-tree)))))
