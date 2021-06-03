(ns org-parser.integration.section-test
  (:require [clojure.string :as str]
            [org-parser.parser :as parser]
            [org-parser.transform :as transform]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])))


(defn- t [& l] (str/join "\n" l))


(def samples
  [{:input
    (t "* hello world"
       "this is the first section"
       "this line has *bold text*")

    :ast
    [:S
     [:headline [:stars "*"] [:text [:text-normal "hello world"]]]
     [:content-line [:text [:text-normal "this is the first section"]]]
     [:content-line
      [:text
       [:text-normal "this line has "]
       [:text-sty-bold "bold text"]]]]

    :result
    {:headlines
     [{:headline
       {:level 1,
        :title [[:text-normal "hello world"]],
        :planning [],
        :tags []},
       :section
       {:ast
        [[:text [:text-normal "this is the first section"]]
         [:text
          [:text-normal "this line has "]
          [:text-sty-bold "bold text"]]],
        :raw ["this is the first section" "this line has *bold text*"]}}]}}

   ;; next sample here:
   ;; {:input ...
   ;;  :ast ...
   ;;  :result ...}
   ])


(deftest section
  (doseq [{:keys [input ast result output]} samples]
    (let [ast* (parser/parse input)]
      (testing "AST matches expected AST" (is (= ast ast*)))
      (testing "transformed AST matches expected transformed AST" (is (= result (transform/transform ast*)))))))


#_(-> samples first :input parser/parse transform/transform)
