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
