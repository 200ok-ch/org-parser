(ns org-parser.transform-test
  (:require [org-parser.transform :as sut]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])))


(def props
  [[:stars "*"] [:title "hello" "world"]])


(deftest property
  (testing "helper fn"
    (is (= ["hello" "world"]
           (#'sut/property :title props)))))

(deftest merge-consecutive-text-normal
  (testing "works for simple case with spaces"
    (is (= [:text [:text-normal "a bc"]]
           (apply #'sut/merge-consecutive-text-normal [[:text-normal "a "] [:text-normal "b"] [:text-normal "c"]])))))

(deftest heading-with-tags
  (testing "heading with tags"
    (is (= [[:text-normal "title"] ["_" "tag1"]]
           (#'sut/extract-tags [:text-normal "title  :_:tag1:"])))))

(deftest transform-uses-span-metadata-for-raw-extraction
  (let [raw "* h\nline one\nline two"
        ast (with-meta
              [:S
               (with-meta [:headline [:level 1] [:text [:text-normal "h"]]] {:span [0 3]})
               (with-meta [:content-line [:text [:text-normal "line one"]]] {:span [4 12]})
               (with-meta [:content-line [:text [:text-normal "line two"]]] {:span [13 21]})]
              {:raw raw})]
    (is (= ["line one" "line two"]
           (-> (sut/transform ast)
               :headlines
               first
               :section
               :raw)))))
