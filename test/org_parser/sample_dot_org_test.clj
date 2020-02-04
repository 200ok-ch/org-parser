(ns org-parser.sample-dot-org-test
  (:require [org-parser.parser :as parser]
            #?(:clj [clojure.test :as t :refer :all]
               :cljs [cljs.test :as t :include-macros true])))

(deftest sample-file
  (let [parse #(parser/org %)]
    (let [sample (parse (slurp (.concat (System/getProperty "user.dir") "/test/org_parser/sample.org")))]
      (testing "in buffer settings"
        (let [[first in-buffer-heading1 in-buffer-heading2] sample]
          (is (= in-buffer-heading1
                 [ :keyword-line [ :keyword-key "TODO" ] [ :keyword-value "TODO | DONE" ] ]))
          (is (= in-buffer-heading2
                 [ :keyword-line [ :keyword-key "TODO" ] [ :keyword-value "START INPROGRESS STALLED | FINISHED" ] ])))))))

