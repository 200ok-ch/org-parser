(ns org-parser.structure-test
  (:require [org-parser.parser :as parser]
            [clojure.walk :as walk]
            #?(:clj [clojure.test :as t :refer :all]
               :cljs [cljs.test :as t :include-macros true])))

(def parse #(parser/structure %))

(defn input-file [orgfile]
  (slurp (str (System/getProperty "user.dir") "/test/fixtures/" orgfile)))

(defn heading? [vec]
  (= :heading (first vec)))

(deftest basic
  (testing "an empty file"
    (is (= [:document] (parse ""))))
  (testing "a file with one section"
    (is (= [:document
            [:section [:paragraph [:content "Foo."]]]]
           (parse "Foo."))))
  (testing "a file with one heading"
    (is (= [:document [:heading [:stars "*"] [:title "Heading"]]]
           (parse "* Heading")))))

(deftest simpleOrgFile
  (let [document (parse (input-file "basic-structure.org"))
        contents (rest document)]
    (testing "it has an initial section"
      (let [[first-section] contents
            [type] first-section]
        (is (= :section type))))
    (testing "it has two top level headings"
      ;; expected to be red
      (let [top-headings (walk/walk #(if (= :heading (first %)) 1 0) identity contents)]
        (is (= 2 (apply + top-headings)))))))
