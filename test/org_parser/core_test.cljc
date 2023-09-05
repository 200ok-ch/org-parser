(ns org-parser.core-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:cljs [cljs-node-io.core :refer [slurp]])
            [org-parser.core :as core]))

(deftest full-round-trip
  (testing "minimal"
    (let [minimal (slurp "test/org_parser/fixtures/minimal.org")]
      (is (= minimal (-> minimal core/read-str core/write-str))))))


(deftest headline-data
  (is (= (core/read-str "* foo bar")
         {:headlines [{:headline {:level 1,
                                  :title [[:text-normal "foo bar"]],
                                  :planning [],
                                  :keyword nil,
                                  :priority nil,
                                  :commented? false
                                  :tags []}}]}))
  (is (= (core/read-str "* TODO foo bar")
         {:headlines [{:headline
                       {:level 1,
                        :title [[:text-normal "foo bar"]],
                        :planning [],
                        :keyword "TODO",
                        :priority nil,
                        :commented? false
                        :tags []}}]}))
  (is (= (core/read-str "* COMMENT foo bar")
         {:headlines [{:headline
                       {:level 1,
                        :title [[:text-normal "foo bar"]],
                        :planning [],
                        :keyword nil,
                        :priority nil,
                        :commented? true
                        :tags []}}]}))
  (is (= (core/read-str "* TODO COMMENT foo bar")
         {:headlines [{:headline
                       {:level 1,
                        :title [[:text-normal "foo bar"]],
                        :planning [],
                        :keyword "TODO",
                        :priority nil,
                        :commented? true
                        :tags []}}]}))
  (is (= (core/read-str "* [#B] foo bar")
         {:headlines [{:headline {:level 1,
                                  :title [[:text-normal "foo bar"]],
                                  :planning [],
                                  :keyword nil,
                                  :priority "B",
                                  :commented? false
                                  :tags []}}]}))
  (is (= (core/read-str "* TODO [#B] foo bar")
         {:headlines [{:headline {:level 1,
                                  :title [[:text-normal "foo bar"]],
                                  :planning [],
                                  :keyword "TODO",
                                  :priority "B",
                                  :commented? false
                                  :tags []}}]})))
