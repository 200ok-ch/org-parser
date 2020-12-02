(ns org-parser.core-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:cljs [cljs-node-io.core :refer [slurp]])
            [org-parser.core :as core]))

(def ^:private sample
  (slurp "test/org_parser/fixtures/bold_text.org"))

(deftest full-round-trip
  (testing "read then write equals identity"
    (is (= sample (-> sample core/read-str core/write-str)))))

(deftest read-str
  (testing "reads the string"
    (is (= [[:headline {:level 1, :title "Main header (required for parser atm)"}]
            [:content "*This is bold*: Hello spec!\n\n*Bold* text can also be just one word.\n\n*This is also bold*: And here goes for some more text which even\n includes more *bold* statements.\n"]]
           (-> sample core/read-str)))))
