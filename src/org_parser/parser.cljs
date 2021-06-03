(ns org-parser.parser
  (:require [instaparse.core :as insta :refer-macros [defparser]]
            ;; [cljs.nodejs :as nodejs]
            ))

(def fs (js/require "fs"))

(defparser parser (.readFileSync fs "resources/org.ebnf" "utf8"))

(defn parse [& args]
  (-> (apply parser args)
      (vary-meta merge {:raw (first args)})))
