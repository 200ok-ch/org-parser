(ns org-parser.parser-test
  (:require [org-parser.parser :as parser]
            #?(:clj [clojure.test :as t :refer :all]
               :cljs [cljs.test :as t :include-macros true])))

(deftest tags
  (testing "a tag"
    (is (= [:tags "a"] (parser/org ":a:" :start :tags)))))
