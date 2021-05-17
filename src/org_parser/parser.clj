(ns org-parser.parser
  (:require [instaparse.core :as insta]))


(defn org [& args]
  (-> "org.ebnf"
      clojure.java.io/resource
      insta/parser
      (apply args)
      (vary-meta merge {:raw (first args)})))
