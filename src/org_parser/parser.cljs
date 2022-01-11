(ns org-parser.parser
  (:require-macros [org-parser.macros :as macros])
  (:require [instaparse.core :as insta :refer-macros [defparser]]))

(defparser parser (macros/inline "org.ebnf"))

(defn parse [& args]
  (-> (apply parser args)
      (vary-meta merge {:raw (first args)})))
