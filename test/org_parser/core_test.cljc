(ns org-parser.core-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:cljs [cljs-node-io.core :refer [slurp]])
            [org-parser.core :as core]))

(deftest full-round-trip
  (testing "minimal"
    (let [minimal (slurp "test/org_parser/fixtures/minimal.org")]
      (is (= minimal (-> minimal core/read-str core/write-str))))))
