(ns org-parser.core
  (:require [instaparse.core :as insta]))


(def parse-org (insta/parser (clojure.java.io/resource "org.bnf")))

;; (insta/defparser parse-org "org.bnf")

(parse-org "a")
