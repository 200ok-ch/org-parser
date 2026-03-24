(ns org-parser.parser-contract-test
  (:require [org-parser.parser :as parser]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])))

(deftest parser-failure-predicate
  (testing "failure? returns true for parse errors"
    (is (parser/failure? (parser/parse "" :start :text-entity))))
  (testing "failure? returns false for successful parses"
    (is (not (parser/failure? (parser/parse "* headline" :start :headline))))))

(deftest parser-default-antlr-contract
  (let [result (parser/parse "* hello" :start :headline)]
    (is (= [:headline [:stars "*"] [:text [:text-normal "hello"]]] result)
        (str result))
    (is (= :antlr (-> result meta :backend-used)))))

(deftest parser-unsupported-start-contract
  #?(:cljs
     (testing "cljs reports unsupported start"
        (let [result (parser/parse "hello" :start :title)]
          (is (parser/failure? result)
              (str result))
          (is (= :unsupported-start (:reason result))
              (str result))))
      :clj
      (testing "clj reports unsupported start"
        (let [result (parser/parse "hello" :start :title)]
          (is (parser/failure? result)
              (str result))
          (is (= :unsupported-start (:reason result))
             (str result))))))
