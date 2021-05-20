(ns org-parser.parser
  (:require [instaparse.core :as insta]))

(def parser
  (-> "org.ebnf"
      clojure.java.io/resource
      insta/parser))


(defn org [& args]
  (-> parser
      (apply args)
      (vary-meta merge {:raw (first args)})))
