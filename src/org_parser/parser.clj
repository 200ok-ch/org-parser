(ns org-parser.parser
  (:require [instaparse.core :as insta :refer [defparser]]))

(defparser parser "resources/org.ebnf")

(defn- ensure-optimize-memory [options]
  (if (some #{:optimize} (take-nth 2 options))
    options
    (concat options [:optimize :memory])))

(defn parse [raw & options]
  (-> parser
      (apply raw (ensure-optimize-memory options))
      (vary-meta merge {:raw raw})))
