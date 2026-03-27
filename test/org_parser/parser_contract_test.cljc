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

(deftest parser-metadata-contract
  (let [raw "* headline\nbody"
        result (parser/parse raw)]
    (is (= :S (first result)))
    (is (= raw (-> result meta :raw)))
    (is (= :antlr (-> result meta :backend-used)))))

(deftest parser-supported-start-rules-contract
  (let [supported (parser/supported-start-rules)]
    (is (set? supported))
    (is (contains? supported :S))
    (is (contains? supported :headline))
    (is (contains? supported :table))))

(deftest parser-invalid-options-contract
  #?(:clj
     (do
       (is (thrown-with-msg?
            clojure.lang.ExceptionInfo
            #"key/value pairs"
            (parser/parse "hello" :start)))
       (is (thrown-with-msg?
            clojure.lang.ExceptionInfo
            #"unsupported parse option"
            (parser/parse "hello" :backend :antlr)))
       (is (thrown-with-msg?
            clojure.lang.ExceptionInfo
            #"must be a keyword"
            (parser/parse "hello" :start "headline"))))
     :cljs
     (do
       (is (try
             (parser/parse "hello" :start)
             false
             (catch js/Error e
               (re-find #"key/value pairs" (.-message e)))))
       (is (try
             (parser/parse "hello" :backend :antlr)
             false
             (catch js/Error e
               (re-find #"unsupported parse option" (.-message e)))))
       (is (try
             (parser/parse "hello" :start "headline")
             false
             (catch js/Error e
               (re-find #"must be a keyword" (.-message e))))))))

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
