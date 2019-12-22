(ns org-parser.parser
  (:require [instaparse.core :as insta]))


(def org (insta/parser (clojure.java.io/resource "org.ebnf")))
